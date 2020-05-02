extern crate proc_macro;

use proc_macro2::TokenTree;
use proc_macro_hack::proc_macro_hack;
use quote::format_ident;
use std::ops::Range;
use syn::parse_macro_input;

struct SeqDecl {
    var_name: proc_macro2::Ident,
    range: Range<usize>,
    content: proc_macro2::TokenStream,
}

impl syn::parse::Parse for SeqDecl {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let var_name: syn::Ident = input.parse()?;
        input.parse::<syn::Token![in]>()?;
        let range_start: syn::LitInt = input.parse()?;
        let range_start = range_start.base10_parse()?;

        let lookahead = input.lookahead1();
        let is_inclusive;

        // "..=" must be checked before ".." as if we have "..=",  peek(syn::Token![..]) will also match.
        if lookahead.peek(syn::Token![..=]) {
            input.parse::<syn::Token![..=]>().unwrap();
            is_inclusive = true;
        } else if lookahead.peek(syn::Token![..]) {
            input.parse::<syn::Token![..]>().unwrap();
            is_inclusive = false;
        } else {
            return Err(lookahead.error());
        }

        let range_end: syn::LitInt = input.parse()?;
        let mut range_end = range_end.base10_parse()?;
        if is_inclusive {
            range_end += 1;
        }

        let content;
        syn::braced!(content in input);

        Ok(SeqDecl {
            var_name,
            range: range_start..range_end,
            content: content.parse()?,
        })
    }
}

fn stream_has_grouping(stream: proc_macro2::TokenStream) -> bool {
    use proc_macro2::Delimiter;
    let mut prev_was_pound = false;
    stream.into_iter().any(|tt| {
        if is_pound(&tt) {
            prev_was_pound = true;
        } else if prev_was_pound {
            match tt {
                TokenTree::Group(group) if group.delimiter() == Delimiter::Parenthesis => {
                    return true
                }
                _ => {}
            };
            prev_was_pound = false;
        }
        match tt {
            TokenTree::Group(group) => stream_has_grouping(group.stream()),
            _ => false,
        }
    })
}

fn is_var_name(tt: &TokenTree, var_name: &proc_macro2::Ident) -> bool {
    match tt {
        TokenTree::Ident(ident) if ident == var_name => true,
        _ => false,
    }
}

fn extract_ident(tt: &TokenTree) -> Option<&proc_macro2::Ident> {
    match tt {
        TokenTree::Ident(ident) => Some(ident),
        _ => None,
    }
}

fn is_pound(tt: &TokenTree) -> bool {
    match tt {
        TokenTree::Punct(punct) if punct.as_char() == '#' => true,
        _ => false,
    }
}

fn replace_in_tt(var_name: &proc_macro2::Ident, value: usize, tt: TokenTree) -> TokenTree {
    match tt {
        TokenTree::Group(group) => TokenTree::Group(proc_macro2::Group::new(
            group.delimiter(),
            replace_in_stream(var_name, value, group.stream()),
        )),
        TokenTree::Ident(ident) => {
            if var_name == &ident {
                TokenTree::Literal(proc_macro2::Literal::usize_unsuffixed(value))
            } else {
                TokenTree::Ident(ident)
            }
        }
        TokenTree::Punct(_) | TokenTree::Literal(_) => tt,
    }
}

fn replace_in_stream(
    var_name: &proc_macro2::Ident,
    value: usize,
    stream: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let mut vec: Vec<_> = stream.into_iter().collect();
    while let Some(pos) = vec.iter().position(|tt| is_var_name(&tt, var_name)) {
        let mut did_replace = false;
        if pos > 1 && is_pound(&vec[pos - 1]) {
            if let Some(to_concat_to) = extract_ident(&vec[pos - 2]) {
                vec[pos] = TokenTree::Ident(format_ident!("{}{}", to_concat_to, value));
                vec.drain(pos - 2..pos);
                did_replace = true;
            }
        }
        if !did_replace {
            vec[pos] = TokenTree::Literal(proc_macro2::Literal::usize_unsuffixed(value));
        }
    }
    vec.into_iter()
        .map(|tt| replace_in_tt(var_name, value, tt))
        .collect()
}

fn repetition(
    var_name: &proc_macro2::Ident,
    range: Range<usize>,
    stream: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    range
        .flat_map(|value| replace_in_stream(&var_name, value, stream.clone()))
        .collect()
}

fn replace_grouping(
    var_name: &proc_macro2::Ident,
    range: Range<usize>,
    stream: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    use proc_macro2::Delimiter;
    let mut iter = stream.into_iter().peekable();
    let mut output = proc_macro2::TokenStream::new();
    while let Some(tt) = iter.next() {
        let mut did_append = false;
        if is_pound(&tt) {
            match iter.peek() {
                Some(TokenTree::Group(group)) if group.delimiter() == Delimiter::Parenthesis => {
                    let src = group.stream();
                    let _ = iter.next().unwrap();
                    output.extend(repetition(var_name, range.clone(), src));
                    iter.next().unwrap(); // skip the '*' after the closing paren (TODO: Check it's a *)
                    did_append = true;
                }
                _ => {}
            }
        }
        if !did_append {
            let tt = match tt {
                TokenTree::Group(group) => TokenTree::Group(proc_macro2::Group::new(
                    group.delimiter(),
                    replace_grouping(var_name, range.clone(), group.stream()),
                )),
                _ => tt,
            };
            output.extend(std::iter::once(tt));
        }
    }
    output.into_iter().collect()
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let decl = parse_macro_input!(input as SeqDecl);
    let output = if stream_has_grouping(decl.content.clone()) {
        replace_grouping(&decl.var_name, decl.range, decl.content.clone())
    } else {
        repetition(&decl.var_name, decl.range, decl.content.clone())
    };

    output.into()
}

#[proc_macro_hack]
pub fn eseq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    seq(input)
}
