#![cfg_attr(feature = "nightly", warn(clippy::pedantic))]

#![recursion_limit = "128"]

//! **Do not use this crate directly, and do not push it to upstream.**
//!
//! See [pbirch::enums](../pbirch/enums/index.html).

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use] extern crate quote;
extern crate syn;

use std::iter;

use proc_macro2::Span;
use proc_macro::TokenStream;
use quote::ToTokens;
use syn::*;

type Args = punctuated::Punctuated<NestedMeta, token::Comma>;

struct ArgsWrapper {
    args: Args,
}

impl syn::parse::Parse for ArgsWrapper {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        Args::parse_terminated(input).map(|args| ArgsWrapper { args })
    }
}

/// The code generator
#[allow(non_snake_case)]
#[proc_macro_attribute]
pub fn EnumRepr(
    args: TokenStream,
    input: TokenStream
) -> TokenStream {
    let input = syn::parse::<ItemEnum>(input)
        .expect("#[EnumRepr] must only be used on enums");
    validate(&input.variants);

    let (repr_ty, implicit, derive) = get_repr_type(args);
    let compiler_repr_ty = match repr_ty.to_string().as_str() {
        "i8" | "i16" | "i32" | "i64" | "i128"
        | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => repr_ty.clone(),
        _ => Ident::new(&"isize", Span::call_site())
    };

    let new_enum = convert_enum(&input, &compiler_repr_ty, implicit);
    let mut ret: TokenStream = match derive {
        true => quote! {
            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        },
        false => quote! { },
    }.into();
    
    let new: TokenStream = new_enum.into_token_stream().into();
    ret.extend(new);

    let gen = generate_code(&input, &repr_ty);
    ret.extend(gen);
    ret
}

fn generate_code(input: &ItemEnum, repr_ty: &Ident) -> TokenStream {
    let ty = input.ident.clone();
    let (names, discrs) = extract_variants(input);
    let vars_len = input.variants.len();

    let (names2, discrs2, discrs3) =
        (names.clone(), discrs.clone(), discrs.clone());
    let repr_ty2 = repr_ty.clone();
    let repr_ty3 = repr_ty.clone();
    let repr_ty4 = repr_ty.clone();
    let ty_repeat = iter::repeat(ty.clone()).take(vars_len);
    let ty_repeat2 = ty_repeat.clone();
    let repr_ty_repeat = iter::repeat(repr_ty.clone()).take(vars_len);
    let repr_ty_repeat2 = repr_ty_repeat.clone();
    let repr_ty_repeat3 = repr_ty_repeat.clone();

    let generics_tuple = input.generics.split_for_impl();
    let (impl_generics, ty_generics, where_clause) = generics_tuple.clone();
    let (impl_generics2, ty_generics2, where_clause2) = generics_tuple;

    let ret: TokenStream = quote! {
        impl #impl_generics Enum<#repr_ty4> for #ty #ty_generics #where_clause {
            fn repr(self) -> #repr_ty2 {
                match self {
                    #( #ty_repeat2::#names2 => #discrs2 as #repr_ty_repeat ),*
                }
            }

            fn from_repr(x: #repr_ty3) -> Option<#ty> {
                match x {
                    #( x if x == #discrs as #repr_ty_repeat2 => Some(#ty_repeat::#names), )*
                    _ => None
                }
            }
        }

        impl #impl_generics2 #ty #ty_generics2 #where_clause2 {
            #[doc(hidden)]
            #[allow(dead_code)]
            fn _enum_repr_typecheck() {
                #( let _x: #repr_ty_repeat3 = #discrs3; )*
                panic!("don't call me!")
            }
        }
    }.into();
    ret
}

fn extract_variants(input: &ItemEnum) -> (Vec<Ident>, Vec<Expr>) {
    let mut prev_expr: Option<Expr> = None;
    let (names, discrs): (Vec<_>, Vec<_>) = input.variants.iter()
        .map(|x| {
            let expr = match x.discriminant.as_ref() {
                Some(discr) => discr.1.clone(),
                None => match prev_expr {
                    Some(ref old_expr) => parse_quote!( 1 + #old_expr ),
                    None => parse_quote!( 0 ),
                }
            };
            prev_expr = Some(expr.clone());
            ( x.ident.clone(), expr )
        }).unzip();
    (names, discrs)
}

fn get_repr_type(args: TokenStream) -> (Ident, bool, bool) {
    let mut repr_type = None;
    let mut implicit = true;
    let mut derive = true;
    let args = syn::parse::<ArgsWrapper>(args)
        .expect("specify repr type in format \"#[EnumRepr]\"").args;
    args.iter().for_each(|arg| {
            match arg {
                NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                    ident, lit, ..
                })) => {
                    let param = ident.to_string();
                    if param == "type" {
                        repr_type = match lit {
                            Lit::Str(repr_ty) => Some(Ident::new(
                                &repr_ty.value(),
                                Span::call_site()
                            )),
                            _ => panic!("\"type\" parameter must be a string")
                        }
                    } else if param == "implicit" {
                        implicit = match lit {
                            Lit::Bool(imp) => imp.value,
                            _ => panic!("\"implicit\" parameter must be bool")
                        }
                    } else if param == "derive" {
                        derive = match lit {
                            Lit::Bool(der) => der.value,
                            _ => panic!("\"derive\" parameter must be bool")
                        }
                    } else {
                        eprintln!("{}", param);
                        panic!("#[EnumRepr] accepts arguments named \
                            \"type\", \"implicit\", and \"derive\"")
                    }
                },
                _ => panic!("specify repr type in format \
                    \"#[EnumRepr(type = \"TYPE\")]\"")
            }
        });
    match repr_type {
        Some(repr_ty) => (repr_ty, implicit, derive),
        None => panic!("\"type \" parameter is required")
    }
}

fn validate(vars: &punctuated::Punctuated<Variant, token::Comma>) {
    for i in vars {
        match i.fields {
            Fields::Named(_) | Fields::Unnamed(_) =>
                panic!("the enum's fields must \
                    be in the \"ident = discriminant\" form"),
            Fields::Unit => ()
        }
    }
}

fn convert_enum(
    input: &ItemEnum,
    compiler_repr_ty: &Ident,
    implicit: bool
) -> ItemEnum {
    let mut variants = input.variants.clone();

    let mut prev_expr: Option<Expr> = None;
    variants.iter_mut().for_each(|ref mut var| {
        let discr_opt = var.discriminant.clone();
        let (eq, new_expr): (syn::token::Eq, Expr) = match discr_opt {
            Some(discr) => {
                let old_expr = discr.1.into_token_stream();
                (discr.0, parse_quote!( (#old_expr) as #compiler_repr_ty ))
            },
            None => {
                if !implicit {
                    panic!("use implicit = true to enable implicit discriminants")
                }
                let expr = match prev_expr.clone() {
                    Some(old_expr) =>
                        parse_quote!( (1 + (#old_expr)) as #compiler_repr_ty ),
                    None => parse_quote!( 0 as #compiler_repr_ty ),
                };
                (syn::token::Eq { spans: [Span::call_site(),] }, expr)
            },
        };
        prev_expr = Some(new_expr.clone());
        var.discriminant = Some((eq, new_expr));
    });

    let mut attrs = input.attrs.clone();
    attrs.push(parse_quote!( #[repr(#compiler_repr_ty)] ));

    let ret = input.clone();
    ItemEnum {
        variants,
        attrs,
        .. ret
    }
}
