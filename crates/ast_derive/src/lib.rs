use proc_macro::TokenStream;
use quote::{quote, quote_spanned};
use syn::{Data, DeriveInput, Fields, Index, parse_macro_input, spanned::Spanned};

#[proc_macro_derive(WalkAst)]
pub fn derive_walk_ast(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;
    let span = input.span();

    let walk_body = match generate_walk_body(&input.data) {
        Ok(body) => body,
        Err(err) => return err.into_compile_error().into(),
    };

    let expanded = quote_spanned! {span =>
        impl<'ast> crate::walk::Walk<'ast> for #name {
            #[inline(always)]
            fn walk<V: crate::walk::Visitor<'ast> + ?Sized>(
                &self,
                a: &'ast crate::ast::AstArena,
                v: &mut V
            ) {
                #walk_body
            }
        }
    };

    expanded.into()
}

fn generate_walk_body(data: &Data) -> syn::Result<proc_macro2::TokenStream> {
    match data {
        Data::Struct(data_struct) => Ok(generate_fields_walk(&data_struct.fields)),
        Data::Enum(data_enum) => {
            let arms = data_enum
                .variants
                .iter()
                .map(|variant| {
                    let variant_name = &variant.ident;
                    generate_variant_arm(variant_name, &variant.fields)
                })
                .collect::<Vec<_>>();

            Ok(quote! {
                match self {
                    #(#arms)*
                }
            })
        }
        Data::Union(u) => Err(syn::Error::new_spanned(
            u.union_token,
            "WalkAst cannot be derived for unions",
        )),
    }
}

fn generate_fields_walk(fields: &Fields) -> proc_macro2::TokenStream {
    let walk_calls = fields.iter().enumerate().map(|(i, field)| {
        let field_access = match &field.ident {
            Some(ident) => quote! { &self.#ident },
            None => {
                let index = Index::from(i);
                quote! { &self.#index }
            }
        };
        quote! { crate::walk::Walk::walk(#field_access, a, v); }
    });

    quote! { #(#walk_calls)* }
}

fn generate_variant_arm(variant_name: &syn::Ident, fields: &Fields) -> proc_macro2::TokenStream {
    match fields {
        Fields::Unit => {
            quote! { Self::#variant_name => {} }
        }
        Fields::Named(fields_named) => {
            let bindings: Vec<_> = fields_named
                .named
                .iter()
                .map(|f| f.ident.as_ref().unwrap())
                .collect();

            let walk_calls = bindings.iter().map(|name| {
                quote! { crate::walk::Walk::walk(#name, a, v); }
            });

            quote! {
                Self::#variant_name { #(#bindings),* } => {
                    #(#walk_calls)*
                }
            }
        }
        Fields::Unnamed(fields_unnamed) => {
            let bindings: Vec<_> = (0..fields_unnamed.unnamed.len())
                .map(|i| quote::format_ident!("f{}", i))
                .collect();

            let walk_calls = bindings.iter().map(|var| {
                quote! { crate::walk::Walk::walk(#var, a, v); }
            });

            quote! {
                Self::#variant_name(#(#bindings),*) => {
                    #(#walk_calls)*
                }
            }
        }
    }
}
