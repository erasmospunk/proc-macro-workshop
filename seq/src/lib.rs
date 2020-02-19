extern crate proc_macro;

use proc_macro2::{TokenTree, TokenStream};

#[derive(Debug)]
struct Sequence {
    variable: syn::Ident,
    in_token: syn::Token![in],
    from: usize,
    dot_dot: syn::Token![..],
    to: usize,
    brace_token: syn::token::Brace,
    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for Sequence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content: syn::parse::ParseBuffer;
        Ok(Sequence {
            variable: input.parse()?,
            in_token: input.parse()?,
            from: input.parse::<syn::LitInt>()?.base10_parse()?,
            dot_dot: input.parse()?,
            to: input.parse::<syn::LitInt>()?.base10_parse()?,
            brace_token: syn::braced!(content in input),
            body: content.parse()?,
        })
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let sequence = syn::parse_macro_input!(input as Sequence);

    println!("BODY ------------\n\n{:?}\n\n------------", sequence.body);
    let mut output = quote::quote! {};
    // Iterate `from` until `to` and replace all instances of `variable`
    for n in sequence.from..sequence.to {
        println!("----replace_START-----");
        let body = replace_with(sequence.body.clone().into(), &sequence.variable, n);
        println!("----replace_END-----");
        output.extend(body);
    }

    println!("OUTPUT BODY ------------\n\n{:?}\n\n------------", sequence.body);
    output.into()
}

/// Replace `variable` in the `tokens` stream with `value`
fn replace_with(
    tokens: proc_macro2::TokenStream,
    variable: &syn::Ident,
    value: usize,
) -> proc_macro2::TokenStream {
    println!("----replace_with\n\n{:?}\n\n-----", tokens);
    let mut token_out: Vec<proc_macro2::TokenTree> = Vec::with_capacity(16);

    for t in tokens {

        println!("TOKEN = {:?}", t);

        let mut new_token: TokenTree = match t {
            TokenTree::Group(group) => {
                // Save delimiter to recreate the TokenTree
                let delimiter = group.delimiter();
                let replaced = replace_with(group.stream(), variable, value);
                // Convert TokenStream to TokenTree
                proc_macro2::Group::new(delimiter, replaced).into()
            },
            // If token is the variable we are replacing, replace it with `value`.
            TokenTree::Ident(ref var) if var == variable => {
                let mut lit = proc_macro2::Literal::usize_unsuffixed(value);
                lit.set_span(var.span());
                lit.into()
            },
            // Output the rest as is
            rest => {
                rest.into()
            },
        };

        // If `#` symbol is present maybe process an identifier in the form of `foo#VAR#suffix`
        if is_pound_symbol(token_out.last()) {
            println!("CHECK POUND");

            // Pop the pound symbol so we can examine the previous items
            let last_pound = token_out.pop().unwrap();

            // Output a new identifier only if there is an `Ident` preceding the `#` symbol
            if let Some(TokenTree::Ident(_)) = token_out.last() {
                println!("APPLY POUND");
                println!("pre-process new_tree = {:?}", new_token);

                let prefix = token_out.pop().unwrap();
                println!("Last was Ident {:?}", prefix);

                // Create the new identifier
                new_token = proc_macro2::Ident::new(
                    &format!("{}{}", prefix, new_token),
                    prefix.span()
                ).into();
            } else {
                // Nothing to do, put it back
                token_out.push(last_pound);
            }
        }

        println!("new_tree = {:?}", new_token);
        token_out.push(new_token);

        println!("token_out capacity = {:?}", token_out.capacity());
    }

    let out: TokenStream = token_out.into_iter().collect();

    println!("----end replace_with\n\n{:?}\n\n-----", out);

    out
}

fn is_pound_symbol(symbol: core::option::Option<&proc_macro2::TokenTree>) -> bool {
    match symbol {
        Some(TokenTree::Punct(symbol)) => {
            // Check that it's the `#` symbol
            symbol.as_char() == '#' && symbol.spacing() == proc_macro2::Spacing::Alone
        },
        _ => false,
    }
}
