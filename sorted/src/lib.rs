extern crate proc_macro;

use quote::ToTokens;
use syn::parse_macro_input;
use syn::visit_mut::VisitMut;

fn extract_enum<'a>(
    args: &proc_macro2::TokenStream,
    item: &'a syn::Item,
) -> syn::Result<&'a syn::ItemEnum> {
    use syn::Item;
    match item {
        Item::Enum(enu) => Ok(enu),
        _ => Err(syn::Error::new_spanned(
            args,
            "expected enum or match expression",
        )),
    }
}

fn pairs<Iter: Iterator + Clone>(iter: Iter) -> impl Iterator<Item = (Iter::Item, Iter::Item)> {
    let mut clone = iter.clone();
    clone.next();
    iter.zip(clone)
}

fn check_enum_order(enu: &syn::ItemEnum) -> syn::Result<()> {
    let incorrect = match pairs(enu.variants.iter())
        .filter(|(var1, var2)| var2.ident < var1.ident)
        .next()
    {
        Some((_, incorrect)) => incorrect,
        None => return Ok(()),
    };

    let following = enu
        .variants
        .iter()
        .filter(|var| var.ident > incorrect.ident)
        .next()
        .unwrap();

    let message = format!("{} should sort before {}", incorrect.ident, following.ident);
    Err(syn::Error::new_spanned(&incorrect.ident, message))
}

fn make_sure_enum_sorted(args: proc_macro2::TokenStream, item: syn::Item) -> syn::Result<()> {
    let enu = extract_enum(&args, &item)?;
    check_enum_order(&enu)?;
    Ok(())
}

#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut tokens: proc_macro2::TokenStream = input.clone().into();
    let item = parse_macro_input!(input as syn::Item);

    match make_sure_enum_sorted(args.into(), item) {
        Err(err) => tokens.extend(err.to_compile_error().into_iter()),
        Ok(_) => {}
    };
    tokens.into()
}

fn full_path(path: &syn::Path) -> String {
    path.segments
        .iter()
        .map(|segment| segment.ident.to_string())
        .collect::<Vec<_>>()
        .join("::")
}

fn check_match_order(expr: &syn::ExprMatch) -> syn::Result<()> {
    use syn::Pat;

    let filtered_arms: Vec<_> = expr
        .arms
        .iter()
        .filter(|arm| match &arm.pat {
            Pat::Wild(_) => false,
            _ => true,
        })
        .collect();

    let paths: Vec<_> = filtered_arms
        .iter()
        .map(|arm| -> syn::Result<(String, &dyn ToTokens)> {
            match &arm.pat {
                Pat::Ident(id) => Ok((id.ident.to_string(), &id.ident)),
                Pat::Path(path) => Ok((full_path(&path.path), &path.path)),
                Pat::Struct(struc) => Ok((full_path(&struc.path), &struc.path)),
                Pat::TupleStruct(struc) => Ok((full_path(&struc.path), &struc.path)),
                _ => Err(syn::Error::new_spanned(
                    &arm.pat,
                    "unsupported by #[sorted]",
                )),
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;
    let (incorrect_path, toks) = match pairs(paths.iter())
        .filter_map(
            |((s1, _), (s2, path2))| {
                if s2 < s1 {
                    Some((s2, path2))
                } else {
                    None
                }
            },
        )
        .next()
    {
        Some((incorrect_path, spanned)) => (incorrect_path, spanned),
        None => {
            if let Some(wild_idx) = expr.arms.iter().position(|arm| match &arm.pat {
                Pat::Wild(_) => true,
                _ => false,
            }) {
                if wild_idx != expr.arms.len() - 1 {
                    let toks = &expr.arms[wild_idx].pat;
                    return Err(syn::Error::new_spanned(toks, "_ should be last"));
                } else {
                    return Ok(());
                }
            } else {
                return Ok(());
            }
        }
    };

    let following_path = paths
        .iter()
        .filter_map(|(s, _)| if s > incorrect_path { Some(s) } else { None })
        .next()
        .unwrap();

    let message = format!("{} should sort before {}", incorrect_path, following_path);
    Err(syn::Error::new_spanned(toks, message))
}

struct FnChecker {
    errs: Vec<syn::Error>,
}

impl VisitMut for FnChecker {
    fn visit_expr_match_mut(&mut self, expr: &mut syn::ExprMatch) {
        let (sorted, others): (Vec<syn::Attribute>, Vec<syn::Attribute>) = expr
            .attrs
            .drain(..)
            .partition(|attr| attr.path.is_ident("sorted"));
        expr.attrs = others;

        if !sorted.is_empty() {
            if let Err(err) = check_match_order(&expr) {
                self.errs.push(err);
            }
        }
    }
}

#[proc_macro_attribute]
pub fn check(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let mut item = parse_macro_input!(input as syn::ItemFn);
    let mut checker = FnChecker { errs: Vec::new() };
    checker.visit_block_mut(&mut item.block);

    let mut output = item.into_token_stream();
    for err in checker.errs {
        output.extend(err.to_compile_error().into_iter());
    }
    output.into()
}
