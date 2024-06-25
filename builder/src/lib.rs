use proc_macro::TokenStream;
use quote::quote;
use std::fmt;
use syn::{parse_macro_input, DeriveInput, Error};

fn inner_type<'a, 'b>(ty: &'a syn::Type, wrapper_type: &'b str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { segments, .. },
        ..
    }) = ty
    {
        if let Some(syn::PathSegment { ident, arguments }) = segments.first() {
            if ident != wrapper_type {
                return None;
            }

            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                args,
                ..
            }) = arguments
            {
                if let syn::GenericArgument::Type(inner_ty) = args.first().unwrap() {
                    Some(inner_ty)
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn make_error<T: quote::ToTokens, U: fmt::Display>(tokens: T, message: U) -> syn::Error {
    Error::new_spanned(tokens, message)
}

fn each_attribute_name_literal(attrs: Vec<syn::Attribute>) -> Option<Result<syn::Ident, Error>> {
    for attr in attrs.into_iter() {
        if attr.path().is_ident("builder") {
            if let syn::Meta::List(syn::MetaList { ref tokens, .. }) = attr.meta {
                let mut t_tree_iter = tokens.clone().into_iter();

                match t_tree_iter.next().unwrap() {
                    proc_macro2::TokenTree::Ident(ident) => {
                        if ident != proc_macro2::Ident::new("each", ident.span()) {
                            return Some(Err(make_error(
                                attr.meta,
                                r#"expected `builder(each = "...")`"#,
                            )));
                        }
                    }
                    tt => {
                        return Some(Err(Error::new(
                            tt.span(),
                            format!("Expected 'each', found {}", tt),
                        )));
                    }
                };

                match t_tree_iter.next().unwrap() {
                    proc_macro2::TokenTree::Punct(punct) => {
                        if punct.as_char() != '=' {
                            return Some(Err(Error::new(punct.span(), "Expected '='")));
                        }
                    }
                    tt => {
                        return Some(Err(Error::new(
                            tt.span(),
                            format!("Expected '=', found {}", tt),
                        )));
                    }
                };

                if let proc_macro2::TokenTree::Literal(literal) = t_tree_iter.next().unwrap() {
                    return Some(Ok(syn::Ident::new(
                        literal.to_string().trim_matches('"'),
                        proc_macro2::Span::call_site(),
                    )));
                }
            }
        }
    }
    None
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!("Use only on Structs with named fields")
    };

    let fields_opt = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if inner_type(ty, "Option").is_some() {
            quote! { #name: #ty}
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let ty = &f.ty;
        let name = &f.ident;

        if let Some(inner_ty) = inner_type(ty, "Option") {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            if let Some(each_name) = each_attribute_name_literal(f.attrs.clone()) {
                let inner_ty = inner_type(ty, "Vec").expect("Each attribute must be Vec<T>");
                match each_name {
                    Ok(each_name) => {
                        quote! {
                                pub fn #each_name(&mut self, #each_name: #inner_ty) -> &mut Self {
                                    if let Some(ref mut v) = self.#name {
                                        v.push(#each_name);
                                    } else {
                                        self.#name = Some(vec![#each_name]);
                                    }
                                    self
                                }
                        }
                    }
                    Err(err) => err.to_compile_error(),
                }
            } else {
                quote! {
                    pub fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }
        }
    });

    let build_attrs = fields.iter().map(|f| {
        let name = &f.ident;

        if inner_type(&f.ty, "Option").is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            if each_attribute_name_literal(f.attrs.clone()).is_some() {
                quote! {
                    #name: self.#name.clone().unwrap_or(vec![])
                }
            } else {
                quote! {
                    #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
                }
            }
        }
    });

    let new_build_attrs = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {#name: None}
    });

    let expanded = quote! {
        pub struct #bident {
            #(#fields_opt,)*
        }

        impl #bident {
            #(#methods)*

            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(
                    #name {
                        #(#build_attrs,)*
                    }
                )
            }
        }

        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#new_build_attrs,)*
                }
            }
        }

    };

    TokenStream::from(expanded)
}
