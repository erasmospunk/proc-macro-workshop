//#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::{TokenStream};
use proc_macro2::Ident;

use quote::{quote, quote_spanned, format_ident};
use syn::*;
use syn::punctuated::Punctuated;
use syn::token::Comma;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let cmd = &input.ident;
    let cmd_builder = format_ident!("{}Builder", cmd);

    let fields: Vec<(Field, bool, core::option::Option<Ident>)> = if let Data::Struct(
        DataStruct{
            struct_token: _,
            fields: Fields::Named(
                FieldsNamed{
                    brace_token:_,
                    named,
                }
            ),
            semi_token: _
        }) = input.data
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

            // Find if attributes are present
            let mut attribute: core::option::Option<Ident> = core::option::Option::None;
            for attr in &field.attrs {
                if let Ok(Meta::List(meta)) = attr.parse_meta() {
                    if !meta.path.is_ident("builder") { continue; }
                    if meta.nested.len() == 1 {
                        if let NestedMeta::Meta(Meta::NameValue(m)) = &meta.nested[0] {
                            if m.path.is_ident("each") {
                                if let Lit::Str(name) = &m.lit {
                                    attribute = core::option::Option::Some(format_ident!("{}", name.value()));
                                }
                            } else {
                                return syn::Error::new_spanned(
                                    &meta,
                                    "expected `builder(each = \"...\")`"
                                ).to_compile_error().into();
                            }
                        }
                    }
                }
            }

            let mut field: Field = field.clone();
            // For non optional fields, wrap them in Option
            if !optional {
                field.ty = parse_quote! {core::option::Option<#ty>};
            }
            // Clear attributes
            field.attrs.clear();
            fields_tokens.push((field, optional, attribute))
        }
        fields_tokens
    } else {
        unimplemented!("Unexpected structure format");
    };

    let fields_tokens = fields.iter().map(|(f, _, _)| f);
    let cmd_builder_struct = quote! {
        pub struct #cmd_builder {
            #(#fields_tokens),*
        }
    };

    let fields_init_none = fields.iter().map(|(f, _, each)| {
        let i = f.ident.as_ref().unwrap();
        if each.is_none() {
            quote_spanned!(i.span() => #i: core::option::Option::None)
        } else {
            // When dealing with `each` variants assume a vec
            quote_spanned!(i.span() => #i: core::option::Option::Some(Vec::new()))
        }
    });

    let cmd_builder_setters = fields.iter().map(|(f, _, each)| {
        let i = f.ident.as_ref().unwrap();
        // Extract T from Option<T>
        let t = extract_inner_type(&f.ty, "Option".into());

        let one_at_a_time_method = if let core::option::Option::Some(new_i) = each {
            if let GenericArgument::Type(t) = &t[0] {
                // Extract T from Vec<T>
                let new_t = extract_inner_type(t, "Vec".into());
                core::option::Option::Some(quote_spanned!(i.span() => fn #new_i(&mut self, #new_i: #new_t) -> &mut Self {
                    if self.#i.is_none() {
                        self.#i = core::option::Option::Some(Vec::new());
                    }
                    self.#i.as_mut().unwrap().push(#new_i);
                    self
                }))
            } else { core::option::Option::None }
        } else { core::option::Option::None };

        // Generate the normal methods if no `each` attribute is provided OR if the attribute name
        // differs from the structs' member name.
        let all_at_once_method = if each.is_none() || each.as_ref().unwrap() != i {
            quote_spanned!(i.span() => fn #i(&mut self, #i: #t) -> &mut Self {
                self.#i = core::option::Option::Some(#i);
                self
            })
        } else { quote! {} };

        quote! {
            #one_at_a_time_method
            #all_at_once_method
        }
    });

    let build_commands = fields.iter().map(|(f, opt, _)| {
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
            #(#cmd_builder_setters)*

            pub fn build(&mut self) -> core::result::Result<#cmd, std::boxed::Box<dyn Error>> {
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

fn extract_inner_type(ty: &Type, outer_id: String) -> &Punctuated<GenericArgument, Comma> {
    match ty {
        Type::Path(p) => {
            for segment in &p.path.segments {
                if segment.ident != outer_id { continue }
                if let PathArguments::AngleBracketed(a) = &segment.arguments {
                    return &a.args;
                }
            }
            panic!("Unexpected format");
        },
        _ => unimplemented!(),
    }
}