extern crate proc_macro;

use proc_macro2::TokenTree;

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

    let mut output = quote::quote! {};
    // Iterate `from` until `to` and replace all instances of `variable`
    for n in sequence.from..sequence.to {
        let body = replace_with(sequence.body.clone().into(), &sequence.variable, n);
        output.extend(body);
    }
    output.into()
}

/// Replace `variable` in the `tokens` stream with `value`
fn replace_with(
    tokens: proc_macro2::TokenStream,
    variable: &syn::Ident,
    value: usize,
) -> proc_macro2::TokenStream {
    tokens.into_iter().map(|t| {
        let new_tree: TokenTree = match t {
            TokenTree::Group(group) => {
                // Save delimiter to recreate the TokenTree
                let delimiter = group.delimiter();
                let replaced = replace_with(group.stream(), variable, value);
                // Convert TokenStream to TokenTree
                proc_macro2::Group::new(delimiter, replaced).into()
            },
            TokenTree::Ident(ref id) if id == variable => {
                // If identity that we are looking for, replace it with the literal `value`
                proc_macro2::Literal::usize_unsuffixed(value).into()
            },
            // Ignore the rest
            rest => rest.into(),
        };
        new_tree
    }).collect()
}
