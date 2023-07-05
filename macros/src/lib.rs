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


#[proc_macro_attribute]
pub fn mux(_attr:TokenStream, input:TokenStream) -> TokenStream{
    let item_enum:ItemEnum = syn::parse(input.clone()).unwrap();
    let enum_ty = item_enum.ident.clone();
    let enum_ty_snake = enum_ty.to_string().to_case(Case::Snake);
    let enum_generics: syn::Generics = item_enum.generics.clone();

    let mut var_map = HashMap::new();
    item_enum.variants.iter().for_each(|x|{
        let var_span = x.span().unwrap();
        let field_it = x.fields.iter();
        if let (1,Some(field)) = (field_it.clone().count(),field_it.last()){
            if let Some(_origin) = var_map.insert(field.ty.clone(),x.ident.clone()) {
                var_span.error("mux enum should have one to one respondence between variant and field type\n").emit();
            }
        }else{
            var_span.error("mux enum should have 1 field in each variant").emit();
        }
    });

    let macro_name = syn::Ident::new(&(enum_ty_snake+"_match"),Span::call_site());
    let vars = var_map.values();
    let mut output:proc_macro2::TokenStream = input.into();

    // enum_match! macro define
    output.extend(quote!(
        pub macro #macro_name($input:expr => $var:ident,$output:expr){{
            use #enum_ty :: *;
            match $input{
                #(#vars ($var) => {
                    $output
                })*
            }
        }}
    ));

    // From<Type> define
    output.extend(var_map.iter().map(|(ty,var)|{ quote!(
        impl #enum_generics From<#ty> for #enum_ty #enum_generics{
            #[inline(always)]
            fn from(val:#ty) -> Self {
                Self::#var(val)
            }
        }
    )}).fold(quote!(),|mut input,now|{
        input.extend(now);
        input
    }));

    // TryInto<Type>
    output.extend(var_map.iter().map(|(ty,var)|{ quote!(
        impl #enum_generics TryInto<#ty> for #enum_ty #enum_generics{
            type Error = ();
            #[inline(always)]
            fn try_into(self) -> std::result::Result<#ty, Self::Error> {
                use #enum_ty :: *;
                if let #var(val)=self{
                    Ok(val)
                }else{
                    Err(())
                }
            }
        }
    )}).fold(quote!(),|mut input,now|{
        input.extend(now);
        input
    }));


    println!("{:#}",output.to_string());
    output.into()
}