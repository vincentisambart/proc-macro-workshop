extern crate proc_macro;

use quote::quote;
use std::borrow::Cow;
use syn::visit::Visit;
use syn::{parse_macro_input, parse_quote};

fn extract_format(field: &syn::Field) -> syn::Result<Option<String>> {
    for attr in field
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("debug"))
    {
        let value = match attr.parse_meta()? {
            syn::Meta::NameValue(value) => value,
            _ => unimplemented!(),
        };
        match value.lit {
            syn::Lit::Str(s) => return Ok(Some(s.value())),
            _ => unimplemented!(),
        };
    }
    Ok(None)
}

struct TypeDirectlyUsedVisitor<'a> {
    searched: &'a syn::Ident,
    is_used: bool,
}

impl<'a> TypeDirectlyUsedVisitor<'a> {
    fn new(searched: &'a syn::Ident) -> Self {
        TypeDirectlyUsedVisitor {
            searched,
            is_used: false,
        }
    }
}

impl<'a> Visit<'a> for TypeDirectlyUsedVisitor<'a> {
    fn visit_type_path(&mut self, ty_path: &'a syn::TypePath) {
        let segments = &ty_path.path.segments;
        if let Some(last) = segments.last() {
            // Ignore references to type inside PhantomData.
            if last.ident == "PhantomData" {
                return;
            }
        }
        if ty_path.path.is_ident(self.searched) {
            self.is_used = true;
        }
        syn::visit::visit_type_path(self, ty_path);
    }
}

fn should_add_type_param(
    ident: &syn::Ident,
    named_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> bool {
    named_fields.iter().any(|field| {
        let mut visitor = TypeDirectlyUsedVisitor::new(ident);
        visitor.visit_type(&field.ty);
        visitor.is_used
    })
}

struct AssociatedTypesVisitor<'s, 'ast> {
    searched: &'s syn::Ident,
    assoc_ty: Vec<&'ast syn::TypePath>,
}

impl<'s, 'ast> AssociatedTypesVisitor<'s, 'ast> {
    fn new(searched: &'s syn::Ident) -> Self {
        AssociatedTypesVisitor {
            searched,
            assoc_ty: Vec::new(),
        }
    }
}

impl<'s, 'ast> Visit<'ast> for AssociatedTypesVisitor<'s, 'ast> {
    fn visit_type_path(&mut self, ty_path: &'ast syn::TypePath) {
        let segments = &ty_path.path.segments;
        if let Some(first) = segments.first() {
            if segments.len() > 1 && &first.ident == self.searched {
                self.assoc_ty.push(ty_path);
            }
        }
        syn::visit::visit_type_path(self, ty_path);
    }
}

fn associated_types<'a>(
    ident: &syn::Ident,
    named_fields: &'a syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> Vec<&'a syn::TypePath> {
    named_fields
        .iter()
        .flat_map(|field| {
            let mut visitor = AssociatedTypesVisitor::new(ident);
            visitor.visit_type(&field.ty);
            visitor.assoc_ty
        })
        .collect()
}

fn extract_top_level_custom_bounds(input: &syn::DeriveInput) -> syn::Result<Option<String>> {
    use syn::{Meta, NestedMeta};

    for attr in input
        .attrs
        .iter()
        .filter(|attr| attr.path.is_ident("debug"))
    {
        let nested = match attr.parse_meta()? {
            syn::Meta::List(list) => {
                assert!(list.path.is_ident("debug"));
                list.nested
            }
            _ => unimplemented!(),
        };
        assert!(nested.len() == 1);
        let nested_meta = nested.first().unwrap();
        let name_value = match nested_meta {
            NestedMeta::Meta(Meta::NameValue(name_value)) => name_value,
            _ => unimplemented!(),
        };
        assert!(name_value.path.is_ident("bound"));
        match &name_value.lit {
            syn::Lit::Str(s) => return Ok(Some(s.value())),
            _ => unimplemented!(),
        };
    }
    Ok(None)
}

fn derive_debug(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    use syn::{Data, Fields, GenericParam};

    let ident = &input.ident;

    let data = match &input.data {
        Data::Struct(struct_data) => struct_data,
        _ => unimplemented!(),
    };
    let fields = match &data.fields {
        Fields::Named(named_fields) => named_fields,
        _ => unimplemented!(),
    };
    let field_vals = fields.named.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        quote! { &self.#ident }
    });

    let fields_format = fields
        .named
        .iter()
        .map(|field| -> syn::Result<String> {
            let ident = field.ident.as_ref().unwrap();
            let format: Cow<str> = if let Some(format) = extract_format(&field)? {
                Cow::Owned(format)
            } else {
                Cow::Borrowed("{:?}")
            };
            Ok(std::format!("{}: {}", ident, format))
        })
        .collect::<syn::Result<Vec<_>>>()?
        .join(", ");
    let full_format = std::format!("{} {{{{ {} }}}}", ident, fields_format);

    let mut generics = input.generics.clone();
    if let Some(custom_bounds) = extract_top_level_custom_bounds(&input)? {
        let where_clause = generics.make_where_clause();
        where_clause
            .predicates
            .push(syn::parse_str(&custom_bounds)?);
    } else {
        let mut assoc_tys = Vec::new();
        for param in &mut generics.params {
            if let GenericParam::Type(ref mut type_param) = *param {
                if should_add_type_param(&type_param.ident, &fields.named) {
                    type_param.bounds.push(parse_quote!(::std::fmt::Debug));
                }
                assoc_tys.extend(associated_types(&type_param.ident, &fields.named).into_iter());
            }
        }
        if !assoc_tys.is_empty() {
            let where_clause = generics.make_where_clause();
            where_clause.predicates.extend(assoc_tys.into_iter().map(
                |assoc_ty| -> syn::WherePredicate { parse_quote!(#assoc_ty: ::std::fmt::Debug) },
            ));
        }
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let debug = quote! {
        impl #impl_generics ::std::fmt::Debug for #ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                ::std::write!(f, #full_format, #(#field_vals,)*)
            }
        }
    };

    Ok(debug)
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);

    match derive_debug(input) {
        Ok(debug) => debug.into(),
        Err(err) => err.to_compile_error().into(),
    }
}
