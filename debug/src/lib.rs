use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use std::collections::{hash_map::RandomState, HashMap, HashSet};
use syn::{
    parse_macro_input, parse_quote, punctuated::Punctuated, DeriveInput, GenericArgument,
    GenericParam, Generics, PathArguments, Token, WherePredicate,
};

fn make_error<T: quote::ToTokens>(tokens: T) -> syn::Error {
    syn::Error::new_spanned(tokens, "Expected `debug = ...`")
}

fn make_error_bound<T: quote::ToTokens>(tokens: T) -> syn::Error {
    syn::Error::new_spanned(tokens, "Expected `debug(bound = ...)`")
}

fn add_trait_bounds(
    mut generics: Generics,
    type_idents_to_extend: HashSet<&syn::Ident>,
    mut ident_to_bounded_ty: HashMap<syn::Ident, syn::WherePredicate>,
) -> Generics {
    let mut predicate: Option<syn::WherePredicate> = None;

    for param in &mut generics.params {
        if let GenericParam::Type(ref mut type_param) = *param {
            if type_idents_to_extend.contains(&&type_param.ident) {
                predicate = ident_to_bounded_ty.remove(&type_param.ident);

                if predicate.is_none() {
                    type_param.bounds.push(parse_quote!(std::fmt::Debug));
                }
            }
        }
    }

    if let Some(predicate) = predicate {
        let clause = generics.make_where_clause();
        clause.predicates.push(predicate);
    }

    generics
}

fn specified_format(f: &syn::Field) -> Option<Result<String, syn::Error>> {
    let attrs = &f.attrs;

    for attr in attrs.iter() {
        if attribute_format(attr).is_some() {
            return attribute_format(attr);
        }
    }

    None
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

fn ident_wrapped_in_phantom_data(ty: &syn::Type) -> Option<syn::Ident> {
    if let syn::Type::Path(syn::TypePath {
        path: syn::Path { ref segments, .. },
        ..
    }) = ty
    {
        for segment in segments.iter() {
            if segment.ident != "PhantomData" {
                return None;
            };

            if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                ref args,
                ..
            }) = segment.arguments
            {
                if let GenericArgument::Type(syn::Type::Path(syn::TypePath { ref path, .. })) =
                    args.first().unwrap()
                {
                    return Some(path.get_ident().unwrap().clone());
                }
            }
        }
    }

    None
}

fn get_bounded_types_from_attrs(
    attrs: &Vec<syn::Attribute>,
) -> Result<HashMap<syn::Ident, WherePredicate>, syn::Error> {
    let mut result = HashMap::new();

    for attr in attrs.iter() {
        if let syn::Meta::List(ref meta_list) = attr.meta {
            if !meta_list.path.is_ident("debug") {
                return Err(make_error_bound(meta_list));
            }

            let mut t_iter = meta_list.tokens.clone().into_iter();

            if let TokenTree::Ident(ident) = t_iter.next().unwrap() {
                if ident.to_string() != "bound" {
                    return Err(make_error_bound(ident));
                }
            } else {
                return Err(make_error_bound(meta_list));
            }

            if let TokenTree::Punct(punct) = t_iter.next().unwrap() {
                if punct.as_char() != '=' {
                    return Err(make_error_bound(punct));
                }
            } else {
                return Err(make_error_bound(meta_list));
            }

            if let TokenTree::Literal(lit) = t_iter.next().unwrap() {
                let ident = syn::Ident::new("T", lit.span());
                let predicate = syn::parse_str::<WherePredicate>(&format!(
                    "{}",
                    &lit.to_string().trim_matches('"')
                ))?;

                result.insert(ident, predicate);
            } else {
                return Err(make_error_bound(meta_list));
            }
        }
    }

    Ok(result)
}

fn get_associated_type_ident(
    segments: &Punctuated<syn::PathSegment, syn::token::PathSep>,
    generic_idents: &HashSet<syn::Ident>,
) -> Option<(syn::Ident, syn::WherePredicate)> {
    if segments.len() == 1 {
        match segments.first().unwrap().arguments {
            PathArguments::AngleBracketed(ref angle_args) => {
                for arg in angle_args.args.iter() {
                    if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                        ref path,
                        ..
                    })) = arg
                    {
                        let ident = get_associated_type_ident(&path.segments, generic_idents);

                        if ident.is_some() {
                            return ident;
                        }
                    }
                }
            }
            _ => return None,
        }
    } else if segments.len() == 2 {
        let seg_ident: syn::Ident = segments.first().unwrap().ident.clone();

        if generic_idents.contains(&seg_ident) {
            return Some((
                seg_ident.clone(),
                syn::WherePredicate::Type(syn::PredicateType {
                    lifetimes: None,
                    bounded_ty: syn::Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: segments.clone(),
                        },
                    }),

                    colon_token: Token![:](seg_ident.span()),
                    bounds: parse_quote!(std::fmt::Debug),
                }),
            ));
        }
    };

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

    let bounded_types_from_attrs = match get_bounded_types_from_attrs(&ast.attrs) {
        Ok(t) => t,
        Err(err) => return err.to_compile_error().into(),
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

    let idents_wrapped_in_phantom_data: HashSet<syn::Ident, RandomState> = HashSet::from_iter(
        named
            .iter()
            .filter_map(|f| ident_wrapped_in_phantom_data(&f.ty)),
    );

    let generic_idents =
        HashSet::from_iter(ast.generics.params.clone().into_iter().filter_map(|p| {
            if let GenericParam::Type(syn::TypeParam { ident, .. }) = p {
                Some(ident.clone())
            } else {
                None
            }
        }));

    let difference = generic_idents
        .difference(&idents_wrapped_in_phantom_data)
        .into_iter()
        .collect();

    let associated_paths = named.iter().filter_map(|f| {
        if let syn::Type::Path(syn::TypePath {
            path: syn::Path { ref segments, .. },
            ..
        }) = &f.ty
        {
            get_associated_type_ident(segments, &generic_idents)
        } else {
            None
        }
    });

    let generics = add_trait_bounds(
        ast.generics,
        difference,
        HashMap::<syn::Ident, WherePredicate>::from_iter(associated_paths)
            .into_iter()
            .chain(bounded_types_from_attrs)
            .collect(),
    );

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
