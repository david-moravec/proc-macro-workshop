use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use syn::parse::{Parse, ParseStream};
use syn::{parse_macro_input, Ident, LitInt, Result, Token};

struct SeqMacroInput {
    param: Ident,
    from: u64,
    to: u64,
    body: proc_macro2::TokenStream,
}

impl Parse for SeqMacroInput {
    fn parse(input: ParseStream) -> Result<Self> {
        let param = input.parse()?;
        input.parse::<Token![in]>()?;
        let lit: LitInt = input.parse()?;
        let from = lit.base10_parse::<u64>()?;
        input.parse::<Token![..]>()?;
        let lit: LitInt = input.parse()?;
        let to = lit.base10_parse::<u64>()?;
        let body: TokenTree = input.parse()?;

        if let TokenTree::Group(group) = body {
            Ok(SeqMacroInput {
                param,
                from,
                to,
                body: group.stream(),
            })
        } else {
            Err(syn::Error::new_spanned(body, "Expected block of code"))
        }
    }
}

impl SeqMacroInput {
    fn expand_over_range(self) -> proc_macro2::TokenStream {
        let mut result = proc_macro2::TokenStream::new();

        for i in self.from..self.to {
            result.extend(self.expand_once(self.body.clone(), i))
        }

        result
    }

    fn expand_once(
        &self,
        stream: proc_macro2::TokenStream,
        value: u64,
    ) -> proc_macro2::TokenStream {
        let mut result = proc_macro2::TokenStream::new();
        let mut stream_iter = stream.into_iter();

        while let Some(tt) = stream_iter.next() {
            result.extend(self.expand2(value, tt, &mut stream_iter))
        }

        result
    }

    fn expand2(
        &self,
        value: u64,
        token_tree: proc_macro2::TokenTree,
        rest: &mut proc_macro2::token_stream::IntoIter,
    ) -> proc_macro2::TokenStream {
        let token_tree = match token_tree {
            TokenTree::Group(ref group) => {
                let mut replaced = TokenTree::Group(proc_macro2::Group::new(
                    group.delimiter(),
                    self.expand_once(group.stream(), value),
                ));
                replaced.set_span(group.span());
                replaced
            }
            TokenTree::Ident(ref ident) if ident == &self.param => {
                TokenTree::Literal(proc_macro2::Literal::u64_unsuffixed(value))
            }
            TokenTree::Ident(mut ident) => {
                let mut peek = rest.clone();

                match (peek.next(), peek.next()) {
                    (Some(TokenTree::Punct(ref punct)), Some(TokenTree::Ident(ref ident2))) => {
                        if punct.as_char() == '~' && ident2 == &self.param {
                            ident = proc_macro2::Ident::new(
                                &format!("{}{}", ident.to_string(), value),
                                ident.span(),
                            );
                            *rest = peek.clone();
                        }
                    }
                    _ => {}
                };

                TokenTree::Ident(ident)
            }
            tt => tt,
        };

        std::iter::once(token_tree).collect()
    }
}

impl Into<proc_macro2::TokenStream> for SeqMacroInput {
    fn into(self) -> proc_macro2::TokenStream {
        self.expand_over_range()
    }
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as SeqMacroInput);
    let output: proc_macro2::TokenStream = input.into();

    output.into()
}
