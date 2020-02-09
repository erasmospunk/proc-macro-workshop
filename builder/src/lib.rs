//#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{parse_macro_input, parse_quote, DeriveInput, DataStruct, Data, Fields, FieldsNamed, Type, Field};
use quote::{quote, quote_spanned, format_ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let cmd = &input.ident;
    let cmd_builder = format_ident!("{}Builder", cmd);

    let fields: Vec<(&mut Field, bool)> = if let Data::Struct(
        DataStruct{
            struct_token: _,
            fields: Fields::Named(
                FieldsNamed{
                    brace_token:_,
                    named,
                }
            ),
            semi_token: _
        }) = &mut input.data
    {
        let mut fields_tokens = Vec::with_capacity(named.len());
        for field in named {
            let ty = &field.ty;
            // Find out if the field is optional
            let optional = match ty {
                // Check if the type is Option<_>
                Type::Path(p) =>
                    !p.path.segments.is_empty() && p.path.segments[0].ident == "Option",
                _ => false,
            };
            // For non optional fields, wrap them in Option
            if !optional {
                let option_ty: Type = parse_quote! {Option<#ty>};
                field.ty = option_ty;
            }
            fields_tokens.push((field, optional))
        }
        fields_tokens
    } else {
        unimplemented!("Unexpected structure format");
    };

    let fields_tokens = fields.iter().map(|(f, _)| f);
    let cmd_builder_struct = quote! {
        pub struct #cmd_builder {
            #(#fields_tokens),*
        }
    };

    let fields_init_none = fields.iter().map(|(f, _)| {
        let i = f.ident.as_ref().unwrap();
        quote_spanned!(i.span() => #i: None)
    });

//    let cmd_builder_setters = fields.iter().map(|(f, _)| {
//        let i = f.ident.as_ref().unwrap();
//        // TODO extract T from Option<T>
//        let t = &f.ty;
//        quote_spanned!(i.span() => fn #i(&mut self, #i: #t) -> &mut Self {
//            self.#i = Some(#i);
//            self
//        })
//    });

    let build_commands = fields.iter().map(|(f, opt)| {
        let i = f.ident.as_ref().unwrap();
        if *opt {
            quote_spanned!(i.span() => #i: self.#i.clone())
        } else {
            let error_msg = format!("Missing `{}`", i);
            quote_spanned!(i.span() => #i: self.#i.as_ref().ok_or(#error_msg)?.clone())
        }
    });

    let impls = quote! {
        impl #cmd {
            pub fn builder() -> #cmd_builder {
                #cmd_builder {
                    #(#fields_init_none),*
                }
            }
        }
        impl #cmd_builder {
//            #(#cmd_builder_setters)*

            fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }
            fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = Some(args);
                self
            }
            fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env = Some(env);
                self
            }
            fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = Some(current_dir);
                self
            }

            pub fn build(&mut self) -> Result<#cmd, Box<dyn Error>> {
                Ok(#cmd {
                    #(#build_commands),*
                })
            }
        }
    };

    let tokens = quote! {
        use std::error::Error;

        #cmd_builder_struct

        #impls
    };

    tokens.into()
}
