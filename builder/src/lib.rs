extern crate proc_macro;

use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields, Type};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive_wrapper(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive(input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn derive(input: DeriveInput) -> Result<proc_macro2::TokenStream, syn::Error> {
    let name = input.ident;
    let builder_name = format_ident!("{}Builder", name);

    let data = match input.data {
        Data::Struct(struct_data) => struct_data,
        _ => unimplemented!(),
    };
    let fields = match data.fields {
        Fields::Named(named_fields) => named_fields,
        _ => unimplemented!(),
    };
    fields
        .named
        .iter()
        .map(check_attrs)
        .collect::<Result<Vec<_>, syn::Error>>()?;

    let field_decls = fields.named.iter().map(|field| {
        let ident = &field.ident;
        let ty = &field.ty;
        if is_optional(ty) || has_attr_each(&field) {
            quote! { #ident: #ty }
        } else {
            quote! { #ident: std::option::Option<#ty> }
        }
    });
    let init_fields = fields.named.iter().map(|field| {
        let ident = &field.ident;
        if has_attr_each(&field) {
            quote! { #ident: std::vec::Vec::new() }
        } else {
            quote! { #ident: std::option::Option::None }
        }
    });
    let setters = fields.named.iter().map(|field| {
        let ident = &field.ident;
        if let Some(attr_ident) = attr_each(&field) {
            quote! {
                fn #attr_ident(&mut self, #attr_ident: std::string::String) -> &mut Self {
                    self.#ident.push(#attr_ident);
                    self
                }
            }
        } else {
            let ty = &field.ty;
            let ty = if let Some(inner) = optional_type(ty) {
                inner
            } else {
                ty
            };
            quote! {
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = std::option::Option::Some(#ident);
                    self
                }
            }
        }
    });
    let unwraps = fields.named.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        let error = format!("{} not set", ident);
        if is_optional(ty) || has_attr_each(&field) {
            quote! {
                let #ident = &self.#ident;
            }
        } else {
            quote! {
                let #ident = match self.#ident {
                    std::option::Option::Some(ref #ident) => #ident,
                    std::option::Option::None => return std::result::Result::Err(#error.into()),
                };
            }
        }
    });
    let clone_fields = fields.named.iter().map(|field| {
        let ident = &field.ident;
        quote! { #ident: #ident.clone() }
    });

    Ok(quote! {
        pub struct #builder_name {
            #(#field_decls,)*
        }

        impl #builder_name {
            #(#setters)*

            pub fn build(&mut self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                #(#unwraps)*

                std::result::Result::Ok(#name {
                    #(#clone_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#init_fields,)*
                }
            }
        }
    })
}

fn is_optional(ty: &Type) -> bool {
    optional_type(ty).is_some()
}

fn optional_type(ty: &Type) -> Option<&Type> {
    use syn::{GenericArgument, Path, PathArguments, TypePath};

    let segments = match ty {
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: None,
                    segments,
                },
        }) => segments,
        _ => return None,
    };
    let segment = match segments.first() {
        Some(segment) if segments.len() == 1 => segment,
        _ => return None,
    };
    if segment.ident != "Option" {
        return None;
    }
    let bracketed = match &segment.arguments {
        PathArguments::AngleBracketed(bracketed) => bracketed,
        _ => return None,
    };
    match bracketed.args.first() {
        Some(GenericArgument::Type(optional_type)) if bracketed.args.len() == 1 => {
            Some(optional_type)
        }
        _ => None,
    }
}

fn has_attr_each(field: &Field) -> bool {
    attr_each(field).is_some()
}

fn check_attrs(field: &Field) -> Result<(), syn::Error> {
    use syn::{Meta, NestedMeta};
    let attrs = &field.attrs;
    for attr in attrs {
        if attr.path.is_ident("builder") {
            let meta = attr.parse_meta().unwrap();
            let meta_list = match meta {
                Meta::List(ref meta_list) => meta_list,
                _ => continue,
            };
            let nested = meta_list.nested.first().unwrap();
            match nested {
                NestedMeta::Meta(Meta::NameValue(name_value)) => {
                    if !name_value.path.is_ident("each") {
                        return Err(syn::Error::new_spanned(
                            meta_list,
                            "expected `builder(each = \"...\")`",
                        ));
                    }
                }
                _ => continue,
            }
        }
    }
    Ok(())
}

fn attr_each(field: &Field) -> Option<Ident> {
    use syn::{Lit, Meta, MetaNameValue, NestedMeta};
    let attrs = &field.attrs;
    attrs
        .iter()
        .filter_map(|attr| {
            if attr.path.is_ident("builder") {
                let nested = match attr.parse_meta().unwrap() {
                    Meta::List(meta) => meta.nested,
                    _ => return None,
                };
                let nested = nested.first().unwrap();
                match nested {
                    NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                        path,
                        eq_token: _,
                        lit: Lit::Str(s),
                    })) if path.is_ident("each") => Some(Ident::new(&s.value(), s.span())),
                    _ => None,
                }
            } else {
                None
            }
        })
        .next()
}
