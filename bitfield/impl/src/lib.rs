extern crate proc_macro;

use quote::ToTokens;
use quote::{format_ident, quote, quote_spanned};
use syn::{parse_macro_input, parse_quote};

fn bitfield_impl(mut item: syn::Item) -> syn::Result<proc_macro2::TokenStream> {
    let struc = if let syn::Item::Struct(ref mut struc) = item {
        struc
    } else {
        return Err(syn::Error::new_spanned(item, "expecting a struct"));
    };
    let name = struc.ident.clone();

    let field_tys = struc.fields.iter().map(|field| field.ty.clone());
    let pos_exprs: Vec<_> = std::iter::once(quote!(0))
        .chain(field_tys.scan(None, |state, ty| {
            let new = match state {
                None => quote!(#ty::BITS),
                Some(size) => quote!((#size + #ty::BITS)),
            };
            *state = Some(new.clone());
            Some(new)
        }))
        .collect();
    let total_bits_size_expr = pos_exprs.last().unwrap();

    let size_checks: Vec<_> = struc
        .fields
        .iter()
        .map(|field: &syn::Field| {
            let attr = match field.attrs.iter().find(|attr| attr.path.is_ident("bits")) {
                Some(attr) => attr,
                None => return Ok(None),
            };

            let meta = attr.parse_meta()?;
            let name_value = match meta {
                syn::Meta::NameValue(name_value) => name_value,
                _ => {
                    return Err(syn::Error::new_spanned(
                        attr,
                        "incorrect bits specification",
                    ))
                }
            };

            let lit = &name_value.lit;
            let lit_int = match lit {
                syn::Lit::Int(lit_int) => lit_int,
                _ => {
                    return Err(syn::Error::new_spanned(
                        name_value.lit,
                        "incorrect bits value",
                    ))
                }
            };

            let ty = &field.ty;
            let span = lit_int.span();
            let check = quote_spanned! {span=>
                const _: [(); #lit] = [(); <#ty>::BITS];
            };

            Ok(Some(check))
        })
        .collect::<syn::Result<_>>()?;

    let accessors: Vec<_> = struc
        .fields
        .iter()
        .enumerate()
        .map(|(i, field)| {
            let field_name = field.ident.as_ref().unwrap();
            let getter_name = format_ident!("get_{}", field_name);
            let setter_name = format_ident!("set_{}", field_name);
            let start_expr = &pos_exprs[i];
            let end_expr = &pos_exprs[i + 1];
            let ty = &field.ty;
            quote! {
                fn #getter_name(&self) -> <#ty as bitfield::Specifier>::OutsideRepr {
                    <#ty as bitfield::Specifier>::try_from(self.get_bit_range(#start_expr..#end_expr)).unwrap()
                }
                fn #setter_name(&mut self, value: <#ty as bitfield::Specifier>::OutsideRepr) {
                    self.set_bit_range(#start_expr..#end_expr, value as _);
                }
            }
        })
        .collect();

    struc.fields = syn::Fields::Named(parse_quote!({ data: [u8; (#total_bits_size_expr) / 8] }));
    let mut output = item.to_token_stream();

    output.extend(quote! {
        impl #name {
            pub const BITS: usize = #total_bits_size_expr;
            pub const BYTES: usize = Self::BITS / 8;

            pub fn new() -> Self {
                Self {
                    data: [0; Self::BYTES],
                }
            }

            pub fn get_bit_range(&self, bit_range: std::ops::Range<usize>) -> u64 {
                let bit_len = bit_range.len();
                let byte_range = (bit_range.start / 8)..((bit_range.end + 7) / 8);
                let start_bit = bit_range.start as isize;
                let end_bit = bit_range.end as isize;
                let end_byte = byte_range.end as isize;
                let last_byte = end_byte - 1;

                let mut ret: u64 = 0;
                for byte_idx in byte_range {
                    let byte = self.data[byte_idx];
                    let shift = ((end_byte * 8) - end_bit) - (last_byte - (byte_idx as isize)) * 8;
                    if shift >= 0 {
                        ret |= (byte as u64) >> shift;
                    } else {
                        ret |= (byte as u64) << -shift;
                    }
                }

                let mask = if bit_len == 64 {
                    u64::MAX
                } else {
                    (1u64 << bit_len) - 1
                };
                ret & mask
            }

            pub fn set_bit_range(&mut self, bit_range: std::ops::Range<usize>, value: u64) {
                let len = bit_range.len() as isize;
                let mask = if len == 64 {
                    0xffffffffu64
                } else {
                    (1u64 << len) - 1
                };
                let value = value & mask;

                let byte_range = (bit_range.start / 8)..((bit_range.end + 7) / 8);
                let end_bit = bit_range.end as isize;
                let end_byte = byte_range.end as isize;
                let last_byte = end_byte - 1;

                for byte_idx in byte_range {
                    let shift = (end_byte * 8) - end_bit - (last_byte - (byte_idx as isize)) * 8;
                    let shifted_value;
                    let shifted_mask;
                    if shift >= 0 {
                        shifted_value = ((value << shift) & 0xff) as u8;
                        shifted_mask = ((mask << shift) & 0xff) as u8;
                    } else {
                        shifted_value = ((value >> -shift) & 0xff) as u8;
                        shifted_mask = ((mask >> -shift) & 0xff) as u8;
                    }

                    let previous_data = self.data[byte_idx];
                    self.data[byte_idx] = (previous_data & !shifted_mask) | shifted_value;
                }
            }

            #(#accessors)*
        }

        const _: () = {
            // Check the size in bits is a multiple of 8.
            let _: bitfield::checks::MultipleOfEightBitsSizeRequired<
                <bitfield::Holder<[(); #name::BITS % 8]> as bitfield::checks::Mod8Name>::Name
            >;
        };

        #(#size_checks)*
    });
    Ok(output)
}

#[proc_macro_attribute]
pub fn bitfield(
    _args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let item = parse_macro_input!(input as syn::Item);
    match bitfield_impl(item) {
        Ok(output) => output,
        Err(err) => err.to_compile_error(),
    }
    .into()
}

#[proc_macro]
pub fn declare_types(_input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let ts: proc_macro2::TokenStream = (1usize..=64usize)
        .flat_map(|i| {
            let name = quote::format_ident!("B{}", i);
            let best_fit = quote::format_ident!("u{}", std::cmp::max(i.next_power_of_two(), 8));
            quote! {
                pub enum #name {}
                impl Specifier for #name {
                    const BITS: usize = #i;
                    type OutsideRepr = #best_fit;

                    fn try_from(value: u64) -> Option<Self::OutsideRepr> {
                        if value >= (1 << #i) {
                            None
                        } else {
                            Some(value as _)
                        }
                    }
                }

                impl BestFit for Holder<[(); #i]> {
                    type Unsigned = #best_fit;
                }
            }
        })
        .collect();

    ts.into()
}

fn derive_impl(input: syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let enum_name = input.ident;
    let enu = match input.data {
        syn::Data::Enum(enu) => enu,
        _ => {
            return Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "BitfieldSpecifier can only be used on enums",
            ))
        }
    };

    if !enu.variants.len().is_power_of_two() {
        return Err(syn::Error::new(
            proc_macro2::Span::call_site(),
            "BitfieldSpecifier expected a number of variants which is a power of 2",
        ));
    }

    let variants: Vec<_> = enu
        .variants
        .iter()
        .map(|variant| variant.ident.clone())
        .collect();

    let bit_size = quote! {
        ((64 - (#((#enum_name::#variants as u64))|*).leading_zeros()) as _)
    };

    let const_decl = variants
        .iter()
        .map(|variant_name| quote!(const #variant_name: u64 = #enum_name::#variant_name as _;));

    let match_arms = variants
        .iter()
        .map(|variant_name| quote!(#variant_name => Some(#enum_name::#variant_name),));

    let max_discriminant_allowed = (variants.len().next_power_of_two() - 1) as u64;
    let max_discriminant_checks = variants.iter().map(|variant| {
        let span = variant.span();
        quote_spanned! {span=>
            const _: () = {
                const B: bool = (#enum_name::#variant as u64) <= #max_discriminant_allowed;
                let _: bitfield::checks::RequireDiscriminantInRange<
                    <bitfield::Holder<[(); B as usize]> as bitfield::checks::Bool
                >::Value>;
            };
        }
    });

    let output = quote! {
        impl bitfield::Specifier for #enum_name {
            const BITS: usize = #bit_size;
            type OutsideRepr = #enum_name;

            #[allow(non_upper_case_globals)]
            fn try_from(value: u64) -> Option<Self::OutsideRepr> {
                #(#const_decl)*
                match value {
                    #(#match_arms)*
                    _ => None,
                }
            }
        }

        #(#max_discriminant_checks)*
    };
    Ok(output)
}

#[proc_macro_derive(BitfieldSpecifier)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    match derive_impl(input) {
        Ok(output) => output,
        Err(err) => err.to_compile_error(),
    }
    .into()
}
