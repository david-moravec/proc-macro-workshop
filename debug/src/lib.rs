use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = ast.ident;
    let named = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!("LOL")
    };

    let fields_debug = named.iter().map(|f| {
        let f_name = &f.ident;

        quote! {
            .field(stringify!(#f_name), &self.#f_name)
        }
    });

    let expanded = quote! {
        impl std::fmt::Debug for #name {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#name))
                    #(#fields_debug)*
                   .finish()
            }
        }
    };

    TokenStream::from(expanded)
}
