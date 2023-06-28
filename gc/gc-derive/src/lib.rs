#![feature(proc_macro_quote,proc_macro_diagnostic)]
#![feature(const_trait_impl)]
use proc_macro::{TokenStream};
use std::collections::HashMap;
use convert_case::{Case, Casing};
use proc_macro2::Span;
use syn::{ItemStruct, ItemEnum, GenericParam, GenericArgument};

use syn::spanned::Spanned;

use quote::{quote};
use syn::punctuated::Punctuated;
/// ```rust
/// pub struct Foo<'gc>{
///     a:usize,
///     b:usize,
///     c:Gc<'gc,usize>,
/// }
///
/// impl<'gc> Collectable for Foo<'gc> {
///     fn trace(&self, hdl: TraceHandle<'_>) {
///         self.a.trace_or_reached(hdl);
///         self.b.trace_or_reached(hdl);
///         self.c.trace_or_reached(hdl);
///     }
///
///     fn need_trace(&self) -> bool {
///         self.a.need_trace()||
///             self.b.need_trace()||
///             self.c.need_trace()
///     }
/// }
/// ```
#[proc_macro_derive(Collectable)]
pub fn derive_collectable_fn(input: TokenStream) -> TokenStream {
    // TODO: implement for enum types
    let item_struct:ItemStruct = syn::parse(input.clone()).expect("gc_derive: only support struct type for now");
    let struct_ident = item_struct.ident.clone();
    let struct_fields = item_struct.fields;
    let struct_generic_params = item_struct.generics.params.clone();
    let struct_where_clause = item_struct.generics.where_clause.clone();
    let struct_generic_arguments = struct_generic_params.iter().fold(Punctuated::new(),|mut pun,param|{
        let arg = match param {
            GenericParam::Type(a) => {
                GenericArgument::Type(syn::parse(quote!(#a).into()).unwrap())
            } ,
            GenericParam::Lifetime(a) => GenericArgument::Lifetime(a.lifetime.clone()),
            GenericParam::Const(a) => {
                let a = syn::parse(quote!(#a).into()).unwrap();
                GenericArgument::Const(a)
            },
        };
        pun.push(arg);
        pun.push_punct(syn::token::Comma::default());
        pun
    });

    let mut output = proc_macro2::TokenStream::new();

    let mut counter = 0;
    let trace_fn_body:proc_macro2::TokenStream = struct_fields
        .iter()
        .fold(String::new(),|out, field|{
            let field = if let Some(ident) = &field.ident{
                ident.to_string()
            }else{
                let field = counter.to_string();
                counter += 1;
                field
            };
            out + format!("self.{}.trace_or_reached(hdl);",field).as_str()
        }).parse().unwrap();

    output.extend(quote!(

        impl < #struct_generic_params > Collectable for #struct_ident < #struct_generic_arguments >
            #struct_where_clause
        {
            fn trace(&self,hdl:gc::TraceHandle<'_>) {
                use gc::impls::CollectableField;
                #trace_fn_body
            }
        }
    ));

    return output.into();
}

