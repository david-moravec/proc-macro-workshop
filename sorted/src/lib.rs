use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{quote, ToTokens};
use std::cmp::Ordering;
use syn::visit_mut::{self, VisitMut};
use syn::ItemFn;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, Arm, ExprMatch, Ident, Item, Meta,
    Path, Variant,
};

struct StripSorted {
    error: Option<syn::Error>,
}

impl VisitMut for StripSorted {
    fn visit_expr_match_mut(&mut self, node: &mut ExprMatch) {
        let mut indices_to_remove: Vec<usize> = vec![];

        for (i, attr) in node.attrs.iter().enumerate() {
            if let Meta::Path(syn::Path { ref segments, .. }) = attr.meta {
                match segments.first() {
                    Some(seg) => {
                        if seg.ident == "sorted" {
                            indices_to_remove.push(i);
                        }
                    }
                    None => {}
                }
            }
        }

        for i in indices_to_remove.into_iter() {
            node.attrs.remove(i);
        }

        match check_expr_match_sorted(&node.arms) {
            Ok(()) => {}
            Err(err) => self.error = Some(err),
        }
        visit_mut::visit_expr_match_mut(self, node);
    }
}

fn check_expr_match_sorted(arms: &Vec<Arm>) -> Result<(), syn::Error> {
    let mut idents: Vec<&Path> = vec![];

    for arm in arms {
        if let syn::Arm {
            pat: syn::Pat::TupleStruct(syn::PatTupleStruct { path, .. }),
            ..
        } = arm
        {
            idents.push(path)
        }
    }

    check_arms_order(idents)
}

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

fn check_idents_order(vec_idents: Vec<&Ident>) -> Result<(), syn::Error> {
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

fn format_path(path: &Path) -> String {
    let split: Vec<String> = path
        .to_token_stream()
        .to_string()
        .split_whitespace()
        .map(|s| s.to_string())
        .collect();

    split.join("")
}

fn check_arms_order(vec_path: Vec<&Path>) -> Result<(), syn::Error> {
    for i in 0..vec_path.len() - 1 {
        let current = vec_path[i];
        let next = vec_path[i + 1];

        if compare_paths_lexographicaly(current, next) == Ordering::Greater {
            let mut vec_idents_sorted: Vec<&Path> = vec_path.clone();
            vec_idents_sorted.sort_by(|v1, v2| compare_paths_lexographicaly(v1, v2));

            let index = vec_idents_sorted
                .iter()
                .position(|n| n == &next)
                .expect(&format!("Ident {:?} not in {:?}", next, vec_idents_sorted));

            let correct_follower = &vec_idents_sorted[index + 1 as usize];

            return Err(syn::Error::new_spanned(
                next,
                format!(
                    "{} should sort before {}",
                    format_path(next),
                    format_path(correct_follower),
                ),
            ));
        }
    }

    Ok(())
}

fn compare_paths_lexographicaly(a: &Path, b: &Path) -> Ordering {
    for (segment_a, segment_b) in a.segments.iter().zip(b.segments.iter()) {
        let ident_a = segment_a.ident.to_string().to_lowercase();
        let ident_b = segment_b.ident.to_string().to_lowercase();

        if ident_a == ident_b {
            continue;
        } else {
            return ident_a.cmp(&ident_b);
        }
    }

    Ordering::Equal
}

fn check_variants_order(variants: &Punctuated<Variant, Comma>) -> Result<(), syn::Error> {
    let vec_idents: Vec<&Ident> = variants.iter().map(|v| &v.ident).collect();
    check_idents_order(vec_idents)
}

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = parse_macro_input!(input as syn::Item);
    let mut tt = quote! {#item};

    match check_order(&item) {
        Ok(()) => {}
        Err(err) => tt.extend(err.into_compile_error()),
    };

    tt.into()
}
#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let mut item = parse_macro_input!(input as ItemFn);

    let mut strip_sorted = StripSorted { error: None };
    strip_sorted.visit_item_fn_mut(&mut item);

    let mut tt = quote! {#item};

    match strip_sorted.error {
        Some(err) => tt.extend(err.to_compile_error()),
        None => {}
    };

    tt.into()
}
