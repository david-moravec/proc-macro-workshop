use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashSet;
use syn::{parse_macro_input, parse_quote, DeriveInput, GenericParam, Generics};

fn specified_format(f: &syn::Field) -> Option<Result<String, syn::Error>> {
    let attrs = &f.attrs;

    for attr in attrs.iter() {
        if attribute_format(attr).is_some() {
            return attribute_format(attr);
        }
    }

    None
}

fn make_error<T: quote::ToTokens>(tokens: T) -> syn::Error {
    syn::Error::new_spanned(tokens, "Expected `debug = ...`")
}

fn add_trait_bounds(
    mut generics: Generics,
    type_idents_to_extend: HashSet<&syn::Ident>,
) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if type_idents_to_extend.contains(&type_param.ident) {
                type_param.bounds.push(parse_quote!(std::fmt::Debug))
            }
        }
    }

    generics
}

fn attribute_format(attr: &syn::Attribute) -> Option<std::result::Result<String, syn::Error>> {
    if let syn::Attribute {
        meta: syn::Meta::NameValue(name_value),
        ..
    } = attr
    {
        if !name_value.path.is_ident("debug") {
            return Some(Err(syn::Error::new_spanned(
                &attr.meta,
                "Expected `debug = ...`",
            )));
        }

        if let syn::Expr::Lit(syn::ExprLit {
            lit: syn::Lit::Str(ref lit_str),
            ..
        }) = name_value.value
        {
            Some(Ok(lit_str.value()))
        } else {
            Some(Err(make_error(&attr.meta)))
        }
    } else {
        None
    }
}

fn ident_not_wrapped_in_phantom_data(ty: &syn::Type) -> Option<&syn::Ident> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        for segment in segments.iter() {
            if segment.ident == "PhantomData" {
                return None;
            };

            return Some(&segment.ident);
        }
    }

    None
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
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

        if let Some(form_res) = specified_format(&f) {
            match form_res {
                Ok(form) => {
                    return quote! {
                        .field(stringify!(#f_name), &format_args!(#form,&self.#f_name))
                    }
                }
                Err(err) => return err.to_compile_error(),
            }
        } else {
            quote! {
                .field(stringify!(#f_name), &self.#f_name)
            }
        }
    });

    let type_idents_not_in_phantom_data = HashSet::from_iter(
        named
            .iter()
            .filter_map(|f| ident_not_wrapped_in_phantom_data(&f.ty)),
    );

    let generics = add_trait_bounds(ast.generics, type_idents_not_in_phantom_data);
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #name #ty_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                fmt.debug_struct(stringify!(#name))
                    #(#fields_debug)*
                   .finish()
            }
        }
    };

    TokenStream::from(expanded)
}
