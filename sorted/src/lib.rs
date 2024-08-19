use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, punctuated::Punctuated, token::Comma, Ident, Item, Variant};

fn check_order(item: &Item) -> Result<(), syn::Error> {
    if let Item::Enum(syn::ItemEnum { variants, .. }) = item {
        check_variants_order(variants)
    } else {
        Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        ))
    }
}

fn check_variants_order(variants: &Punctuated<Variant, Comma>) -> Result<(), syn::Error> {
    let vec_idents: Vec<&Ident> = variants.iter().map(|v| &v.ident).collect();

    for i in 0..vec_idents.len() - 1 {
        let current = vec_idents[i];
        let next = vec_idents[i + 1];

        if current.to_string().to_lowercase() > next.to_string().to_lowercase() {
            let mut vec_idents_sorted: Vec<String> =
                vec_idents.clone().iter().map(|i| i.to_string()).collect();
            vec_idents_sorted.sort_by(|v1, v2| v1.cmp(v2));

            let index = vec_idents_sorted
                .iter()
                .position(|n| n == &next.to_string())
                .unwrap();

            let correct_follower = &vec_idents_sorted[index + 1 as usize];

            return Err(syn::Error::new_spanned(
                next,
                format!(
                    "{:} should sort before {:}",
                    next.to_string(),
                    correct_follower
                ),
            ));
        }
    }

    Ok(())
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = parse_macro_input!(input as syn::Item);

    match check_order(&item) {
        Ok(()) => quote! {#item},
        Err(err) => err.into_compile_error(),
    }
    .into()
}
