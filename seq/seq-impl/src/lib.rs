extern crate proc_macro;

use proc_macro2::TokenTree;
use proc_macro_hack::proc_macro_hack;

#[derive(Debug)]
struct Sequence {
    variable: syn::Ident,
    range: core::ops::Range<usize>,
    body: proc_macro2::TokenStream,
}

impl syn::parse::Parse for Sequence {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let variable = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token![in]>()?;
        let start = input.parse::<syn::LitInt>()?.base10_parse()?;
        input.parse::<syn::Token![..]>()?;
        let end = input.parse::<syn::LitInt>()?.base10_parse()?;
        let range = core::ops::Range { start, end };
        let content: syn::parse::ParseBuffer;
        syn::braced!(content in input);
        let body = content.parse()?;
        Ok(Sequence {
            variable,
            range,
            body,
        })
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let sequence = syn::parse_macro_input!(input as Sequence);
    let output = seq_process_tokens(sequence);
    output.into()
}

#[proc_macro_hack]
pub fn eseq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    seq(input)
}

fn seq_process_tokens(sequence: Sequence) -> proc_macro2::TokenStream {
    let Sequence {
        range,
        variable,
        body,
    } = sequence;
    let (mut output, had_repeat_section) = process_tokens(body, &variable, &range);
    // If no repeat section(s) found, loop the body
    if !had_repeat_section {
        // Replace the output with the looped body
        output = loop_block(output, &variable, &range).into_iter().collect();
    }
    output
}

///
/// Process the stream and return the output tokens and if any repeat section was found
///
fn process_tokens(
    tokens: proc_macro2::TokenStream,
    variable: &syn::Ident,
    range: &core::ops::Range<usize>,
) -> (proc_macro2::TokenStream, bool) {
    let mut had_repeat_section = false;
    let mut tokens_out = std::vec::Vec::with_capacity(16);
    let mut it = tokens.into_iter().peekable();
    while let Some(t) = it.next() {
        if let TokenTree::Group(group) = &t {
            // If we have a repeating section
            if is_symbol('#', tokens_out.last()) && is_symbol('*', it.peek()) {
                had_repeat_section = true;
                tokens_out.pop(); // Remove `#` from output
                it.next(); // Throw away upcoming `*`
                           // Loop this group and add to output tokens
                let looped = loop_block(group.stream().into_iter().collect(), &variable, &range);
                tokens_out.extend(looped);
            } else {
                // Else process this group
                // Save delimiter to recreate the TokenTree
                let delimiter = group.delimiter();
                let (output, had_repeat) = process_tokens(group.stream(), variable, range);
                had_repeat_section |= had_repeat;
                // Convert back to TokenTree Group
                tokens_out.push(proc_macro2::Group::new(delimiter, output).into());
            }
        } else {
            tokens_out.push(t);
        }
    }
    (tokens_out.into_iter().collect(), had_repeat_section)
}

fn loop_block(
    block: proc_macro2::TokenStream,
    variable: &syn::Ident,
    range: &core::ops::Range<usize>,
) -> proc_macro2::TokenStream {
    let mut output = proc_macro2::TokenStream::new();
    // Iterate over the range and replace all instances of `variable`
    for n in range.clone() {
        let replaced = replace_with(block.clone(), &variable, n);
        output.extend(replaced);
    }
    output
}

/// Replace `variable` in the `tokens` stream with `value`
fn replace_with(
    tokens: proc_macro2::TokenStream,
    variable: &syn::Ident,
    value: usize,
) -> proc_macro2::TokenStream {
    let mut token_out = std::vec::Vec::with_capacity(16);

    for t in tokens {
        let mut new_token: TokenTree = match t {
            TokenTree::Group(group) => {
                // Save delimiter to recreate the TokenTree
                let delimiter = group.delimiter();
                let tokens = group.stream().into_iter().collect();
                let replaced = replace_with(tokens, variable, value);
                // Convert back to TokenTree Group
                proc_macro2::Group::new(delimiter, replaced).into()
            }
            // If token is the variable we are replacing, replace it with `value`.
            TokenTree::Ident(ref var) if var == variable => {
                let mut lit = proc_macro2::Literal::usize_unsuffixed(value);
                lit.set_span(var.span());
                lit.into()
            }
            // Output the rest as is
            rest => rest.into(),
        };

        // If `#` symbol is present maybe process an identifier in the form of `foo#VAR#suffix`
        if is_symbol('#', token_out.last()) {
            // Pop the pound symbol so we can examine the previous items
            let last_pound = token_out.pop().unwrap();

            // Output a new identifier only if there is an `Ident` preceding the `#` symbol
            if let Some(TokenTree::Ident(_)) = token_out.last() {
                let prefix = token_out.pop().unwrap();
                // Create the new identifier
                let new_id_name = format!("{}{}", prefix, new_token);
                new_token = proc_macro2::Ident::new(&new_id_name, prefix.span()).into();
            } else {
                // Nothing to do, put it back
                token_out.push(last_pound);
            }
        }
        token_out.push(new_token);
    }

    token_out.into_iter().collect()
}

fn is_symbol(target: char, symbol: core::option::Option<&proc_macro2::TokenTree>) -> bool {
    match symbol {
        Some(TokenTree::Punct(symbol)) => {
            // Check that it's the `target` symbol
            symbol.as_char() == target && symbol.spacing() == proc_macro2::Spacing::Alone
        }
        _ => false,
    }
}
