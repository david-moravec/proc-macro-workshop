use proc_macro::TokenStream;
use syn::parse::Parse;

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let _ = input;
}
