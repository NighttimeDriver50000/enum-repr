#![cfg_attr(feature = "nightly", warn(clippy::pedantic))]

#![recursion_limit = "128"]

//! **Do not use this crate directly, and do not push it to upstream.**
//!
//! See [pbirch::enums](../pbirch/enums/index.html).

extern crate proc_macro;
extern crate proc_macro2;
#[macro_use] extern crate quote;
extern crate syn;
extern crate time;

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
    let t0 = time::precise_time_ns();
    let input = syn::parse::<ItemEnum>(input)
        .expect("#[EnumRepr] must only be used on enums");
    eprintln!("parse input:  {}", time::precise_time_ns() - t0);
    validate(&input.variants);

    let (repr_ty, implicit, derive) = get_repr_type(args);
    let compiler_repr_ty = match repr_ty.to_string().as_str() {
        "i8" | "i16" | "i32" | "i64" | "i128"
        | "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => repr_ty.clone(),
        _ => Ident::new(&"isize", Span::call_site())
    };

    let t1 = time::precise_time_ns();
    let new_enum = convert_enum(&input, &compiler_repr_ty, implicit);
    eprintln!("convert enum: {}", time::precise_time_ns() - t1);
    let mut ret: TokenStream = match derive {
        true => quote! {
            #[derive(Copy, Clone, PartialEq, Eq, Debug)]
        },
        false => quote! { },
    }.into();

    let t2 = time::precise_time_ns(); 
    let new: TokenStream = new_enum.into_token_stream().into();
    eprintln!("into stream:  {}", time::precise_time_ns() - t2);
    ret.extend(new);

    let t3 = time::precise_time_ns();
    let gen = generate_code(&input, &repr_ty);
    eprintln!("genert. code: {}", time::precise_time_ns() - t3);
    ret.extend(gen);

    let tf = time::precise_time_ns();
    eprintln!("TOTAL:        {}", tf - t0);

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
    let mut prev_explicit: Option<Expr> = None;
    let mut implicit_counter = 0;

    let mut is_explicit = false;
    let mut count_explicit = 0;
    let mut count_implicit = 0;
    let mut time_explicit = 0;
    let mut time_implicit = 0;

    variants.iter_mut().for_each(|ref mut var| {
        let t0 = time::precise_time_ns();

        let discr_opt = var.discriminant.clone();
        let (eq, new_expr): (syn::token::Eq, Expr) = match discr_opt {
            Some(discr) => {
                is_explicit = true;

                let old_expr = discr.1.into_token_stream();
                prev_explicit = Some(parse_quote!( (#old_expr) as #compiler_repr_ty ));
                implicit_counter = 0;
                (discr.0, prev_explicit.clone().unwrap())
            },
            None => {
                is_explicit = false;

                if !implicit {
                    panic!("use implicit = true to enable implicit discriminants")
                }
                let expr = match prev_explicit.clone() {
                    Some(old_expr) => {
                        implicit_counter += 1;
                        let lit = syn::Lit::Int(syn::LitInt::new(implicit_counter,
                            syn::IntSuffix::None, Span::call_site()));
                        parse_quote!( (#lit + (#old_expr)) as #compiler_repr_ty )
                    },
                    None => {
                        prev_explicit = Some(parse_quote!( 0 as #compiler_repr_ty ));
                        prev_explicit.clone().unwrap()
                    }
                };
                (syn::token::Eq { spans: [Span::call_site(),] }, expr)
            },
        };
        var.discriminant = Some((eq, new_expr));

        let dt = time::precise_time_ns() - t0;
        if is_explicit {
            count_explicit += 1;
            time_explicit += dt;
        } else {
            count_implicit += 1;
            time_implicit += dt;
        }
    });

    eprintln!("exp ({}): {}", count_explicit, time_explicit);
    eprintln!("    {}", time_explicit / count_explicit);
    eprintln!("imp ({}): {}", count_implicit, time_implicit);
    eprintln!("    {}", time_implicit / count_implicit);

    let mut attrs = input.attrs.clone();
    attrs.push(parse_quote!( #[repr(#compiler_repr_ty)] ));
    let ret = input.clone();

    ItemEnum {
        variants,
        attrs,
        .. ret
    }
}
