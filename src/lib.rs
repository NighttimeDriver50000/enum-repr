// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![cfg_attr(feature = "nightly", warn(clippy::pedantic))]

#![recursion_limit = "128"]

//! **Do not use this crate directly, and do not push it to upstream.**
//!
//! See [vdex::enums](../vdex/enums/index.html).

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
    //let t0 = time::precise_time_ns();
    let input = syn::parse::<ItemEnum>(input)
        .expect("#[EnumRepr] must only be used on enums");
    //eprintln!("parse input:  {}", time::precise_time_ns() - t0);
    validate(&input.variants);

    let (repr_ty, implicit, derive, enable_fast) = get_repr_type(args);
    let (compiler_repr_ty, fast_gen) = match repr_ty.to_string().as_str() {
        "i8" | "i16" | "i32" | "i64" | "isize"
        | "u8" | "u16" | "u32" | "u64" | "usize" => {
            (repr_ty.clone(), enable_fast)
        },
        "i128" | "u128" => {
            if implicit {
                panic!("Implicit not supported for 128-bit reprs!");
            }
            (repr_ty.clone(), false)
        },
        _ => {
            (Ident::new(&"isize", Span::call_site()), false)
        },
    };

    //let t1 = time::precise_time_ns();
    let new_enum = convert_enum(&input, &compiler_repr_ty,
        implicit, derive, fast_gen);
    //eprintln!("convert enum: {}", time::precise_time_ns() - t1);

    //let t2 = time::precise_time_ns(); 
    let mut ret: TokenStream = new_enum.into_token_stream().into();
    //eprintln!("into stream:  {}", time::precise_time_ns() - t2);

    //let t3 = time::precise_time_ns();
    let gen = match fast_gen {
        true => generate_code_fast(&input, &repr_ty),
        false => generate_code(&input, &repr_ty),
    };
    //eprintln!("genert. code: {}", time::precise_time_ns() - t3);
    ret.extend(gen);

    //let tf = time::precise_time_ns();
    //eprintln!("TOTAL:        {}", tf - t0);

    ret
}

fn generate_code_fast(input: &ItemEnum, repr_ty1: &Ident) -> TokenStream {
    //let t0 = time::precise_time_ns();

    let ty = input.ident.clone();
    let vars_len = input.variants.len();
    let (names1, discrs1) = extract_variants(input, true);
    let (names2, discrs2) = (names1.clone(), discrs1.clone());
    let names3 = names1.clone();
    let (repr_ty2, repr_ty3) = (repr_ty1.clone(), repr_ty1.clone());
    let ty_repeat1 = iter::repeat(ty.clone()).take(vars_len);
    let ty_repeat2 = ty_repeat1.clone();
    let ty_repeat3 = ty_repeat1.clone();
    let generics_tuple = input.generics.split_for_impl();
    let (impl_generics, ty_generics, where_clause) = generics_tuple;

    //let t1 = time::precise_time_ns();

    let ret: TokenStream = quote! {
        impl #impl_generics Enum<#repr_ty1> for #ty #ty_generics #where_clause {
            const COUNT: usize = #vars_len;

            const VALUES: &'static [Self] = &[ #( #ty_repeat1::#names1, )* ];

            fn repr(self) -> #repr_ty2 {
                match self {
                    #( #ty_repeat2::#names2 => #discrs1, )*
                }
            }

            fn from_repr(x: #repr_ty3) -> Option<#ty> {
                match x {
                    #( #discrs2 => Some(#ty_repeat3::#names3), )*
                    _ => None,
                }
            }
        }
    }.into();

    //let t2 = time::precise_time_ns();
    //eprintln!("attack of the clone()s: FAST {}", t1 - t0);
    //eprintln!("nevermore! quoth the raven:  {}", t2 - t1);

    ret
}

fn generate_code(input: &ItemEnum, repr_ty: &Ident) -> TokenStream {
    //let t0 = time::precise_time_ns();

    let ty = input.ident.clone();
    let vars_len = input.variants.len();

    let (names1, discrs1) = extract_variants(input, false);
    let names2 = names1.clone();
    let names3 = names1.clone();
    let discrs2 = discrs1.clone();
    let discrs3 = discrs1.clone();

    let repr_ty1 = repr_ty.clone();
    let repr_ty2 = repr_ty.clone();
    let repr_ty3 = repr_ty.clone();
    let ty_repeat1 = iter::repeat(ty.clone()).take(vars_len);
    let ty_repeat2 = ty_repeat1.clone();
    let ty_repeat3 = ty_repeat1.clone();
    let repr_ty_repeat1 = iter::repeat(repr_ty).take(vars_len);
    let repr_ty_repeat2 = repr_ty_repeat1.clone();
    let repr_ty_repeat3 = repr_ty_repeat1.clone();

    let generics_tuple = input.generics.split_for_impl();
    let (impl_generics1, ty_generics1, where_clause1) = generics_tuple.clone();
    let (impl_generics2, ty_generics2, where_clause2) = generics_tuple;

    //let t1 = time::precise_time_ns();

    let ret: TokenStream = quote! {
        impl #impl_generics1 Enum<#repr_ty1> for #ty #ty_generics1 #where_clause1 {
            const COUNT: usize = #vars_len;

            const VALUES: &'static [Self] = &[ #( #ty_repeat1::#names1, )* ];

            fn repr(self) -> #repr_ty2 {
                match self {
                    #( #ty_repeat2::#names2 => #discrs1 as #repr_ty_repeat1, )*
                }
            }

            fn from_repr(x: #repr_ty3) -> Option<#ty> {
                match x {
                    #( x if x == #discrs2 as #repr_ty_repeat2
                        => Some(#ty_repeat3::#names3), )*
                    _ => None,
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

    //let t2 = time::precise_time_ns();
    //eprintln!("attack of the clone()s:      {}", t1 - t0);
    //eprintln!("nevermore! quoth the raven:  {}", t2 - t1);

    ret
}

fn extract_variants(input: &ItemEnum, fast_gen: bool) -> (Vec<Ident>, Vec<Expr>) {
    let mut prev_explicit: Option<Expr> = None;
    let mut implicit_counter = 0;
    let (names, discrs): (Vec<_>, Vec<_>) = input.variants.iter()
        .map(|x| {
            let expr = match x.discriminant.as_ref() {
                Some(discr) => {
                    prev_explicit = Some(discr.1.clone());
                    implicit_counter = 0;
                    prev_explicit.clone().unwrap()
                },
                None => match prev_explicit.clone() {
                    Some(syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Int(ref x),
                        attrs: _,
                    })) if fast_gen == true => {
                        implicit_counter += 1;
                        let lit = syn::Lit::Int(syn::LitInt::new(
                            implicit_counter + x.value(),
                            syn::IntSuffix::None, Span::call_site()));
                        parse_quote!( #lit )
                    },
                    /* // NEEDS NIGHTLY feature(box_patterns)
                    Some(syn::Expr::Unary(syn::ExprUnary {
                        attrs: _,
                        op: syn::UnOp::Neg(_),
                        expr: box syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Int(x),
                            attrs: _,
                        }),
                    })) if fast_gen == true => {
                    */ // WORKAROUND:
                    Some(syn::Expr::Unary(syn::ExprUnary {
                        attrs: _,
                        op: syn::UnOp::Neg(_),
                        ref expr,
                    })) if fast_gen == true => {
                        let x = match **expr {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Int(ref y),
                                attrs: _,
                            }) => y,
                            _ => panic!("I need box matching!"),
                        };
                    // END WORKAROUND
                        implicit_counter += 1;
                        let v = (implicit_counter as i64) - (x.value() as i64);
                        let lit = syn::Lit::Int(syn::LitInt::new(v.abs() as u64,
                            syn::IntSuffix::None, Span::call_site()));
                        if v < 0 {
                            parse_quote!( -#lit )
                        } else {
                            parse_quote!( #lit )
                        }
                    },
                    Some(old_expr) => {
                        implicit_counter += 1;
                        let lit = syn::Lit::Int(syn::LitInt::new(implicit_counter,
                            syn::IntSuffix::None, Span::call_site()));
                        parse_quote!( #lit + (#old_expr) )
                    },
                    None => {
                        prev_explicit = Some(parse_quote!( 0 ));
                        prev_explicit.clone().unwrap()
                    },
                },
            };
            ( x.ident.clone(), expr )
        }).unzip();
    (names, discrs)
}

fn get_repr_type(args: TokenStream) -> (Ident, bool, bool, bool) {
    let mut repr_type = None;
    let mut implicit = true;
    let mut derive = true;
    let mut enable_fast = true;
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
                    } else if param == "fast" {
                        enable_fast = match lit {
                            Lit::Bool(fast) => fast.value,
                            _ => panic!("\"fast\" parameter must be bool")
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
        Some(repr_ty) => (repr_ty, implicit, derive, enable_fast),
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
    implicit: bool,
    derive: bool,
    fast_gen: bool,
) -> ItemEnum {
    let mut variants = input.variants.clone();
    let mut prev_explicit: Option<Expr> = None;
    let mut implicit_counter = 0;

    variants.iter_mut().for_each(|ref mut var| {
        let discr_opt = var.discriminant.clone();
        let (eq, new_expr): (syn::token::Eq, Expr) = match discr_opt {
            Some(discr) => {
                prev_explicit = Some(match fast_gen {
                    true => discr.1.clone(),
                    false => {
                        let old_expr = discr.1.into_token_stream();
                        parse_quote!( (#old_expr) as #compiler_repr_ty )
                    },
                });
                implicit_counter = 0;
                (discr.0, prev_explicit.clone().unwrap())
            },
            None => {
                if !implicit {
                    panic!("use implicit = true to enable implicit discriminants")
                }
                let expr = match prev_explicit.clone() {
                    Some(old_expr) => {
                        implicit_counter += 1;
                        let lit = syn::Lit::Int(syn::LitInt::new(implicit_counter,
                            syn::IntSuffix::None, Span::call_site()));
                        match fast_gen {
                            true => parse_quote!( #lit + (#old_expr) ),
                            false => {
                                parse_quote!( (#lit + (#old_expr)) as #compiler_repr_ty )
                            },
                        }
                    },
                    None => {
                        prev_explicit = Some(match fast_gen {
                            true => parse_quote!( 0 ),
                            false => parse_quote!( 0 as #compiler_repr_ty ),
                        });
                        prev_explicit.clone().unwrap()
                    }
                };
                (syn::token::Eq { spans: [Span::call_site(),] }, expr)
            },
        };
        var.discriminant = Some((eq, new_expr));
    });

    let mut attrs = input.attrs.clone();
    attrs.push(parse_quote!( #[repr(#compiler_repr_ty)] ));
    if derive {
        attrs.push(parse_quote!( #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)] ));
    }
    let ret = input.clone();

    ItemEnum {
        variants,
        attrs,
        .. ret
    }
}
