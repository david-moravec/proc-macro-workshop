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

fn replace_param_with_value(
    param: &Ident,
    value: u64,
    body: proc_macro2::TokenStream,
) -> Result<proc_macro2::TokenStream> {
    let mut tokens = Vec::from_iter(body);

    for i in 0..tokens.len() {
        if i == tokens.len() {
            break;
        };

        match tokens[i] {
            TokenTree::Ident(ref ident) => {
                if ident == param {
                    tokens[i] = TokenTree::Literal(proc_macro2::Literal::u64_unsuffixed(value));
                };
            }
            TokenTree::Group(ref group) => {
                let mut replaced = TokenTree::Group(proc_macro2::Group::new(
                    group.delimiter(),
                    replace_param_with_value(param, value, group.stream())?,
                ));
                replaced.set_span(group.span());
                tokens[i] = replaced;
            }
            TokenTree::Punct(ref punct) => {
                if punct.as_char() == '~' {
                    if tokens[i + 1].to_string() == param.to_string() {
                        let prev = tokens[i - 1].to_string();
                        let new_ident_str = format!("{}{}", prev, value);
                        tokens[i - 1] =
                            TokenTree::Ident(proc_macro2::Ident::new(&new_ident_str, punct.span()));
                        tokens.remove(i + 1);
                        tokens.remove(i);
                    }
                }
            }
            _ => (),
        }
    }

    let result = Ok(proc_macro2::TokenStream::from_iter(tokens));

    // eprintln!("{:#?}", result.as_ref().unwrap());

    result
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let SeqMacroInput {
        param,
        from,
        to,
        body,
    } = parse_macro_input!(input as SeqMacroInput);

    let expanded =
        (from..to).map(
            |val| match replace_param_with_value(&param, val, body.clone()) {
                Ok(replaced) => replaced,
                Err(err) => err.to_compile_error(),
            },
        );

    proc_macro2::TokenStream::from_iter(expanded).into()
}
