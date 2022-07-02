use proc_macro::TokenStream;
use std::error::Error;
use quote::ToTokens;
use std::fmt;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields, Type, PathArguments, Field, GenericArgument, Meta, NestedMeta, Lit, ItemFn};
use syn::spanned::Spanned;
use syn::__private::{Span, TokenStream2};

fn get_inner_type<'a>(f: &'a Field, type_name: &str) -> Option<&'a GenericArgument> {
    let ty = &f.ty;
    match ty {
        Type::Path(ref type_path) => {
            if type_path.path.segments.len() == 1 {
                //TODO fix calls to unwrap
                let path_segment = &type_path.path.segments.first().unwrap();
                let ident = &path_segment.ident;
                if ident == type_name {
                    match &path_segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            if args.args.len() == 1 {
                                let ty = args.args.first().unwrap();
                                Some(ty)
                            } else {
                                None
                            }
                        }
                        _ => { None }
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => { None }
    }
}

#[derive(Debug)]
struct BuilderError {
    details: String
}

impl BuilderError {
    fn new(msg: String) -> BuilderError {
        BuilderError{details: msg.to_string()}
    }
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,"{}",self.details)
    }
}

impl Error for BuilderError {
    fn description(&self) -> &str {
        &self.details
    }
}

fn get_attribute_name(attr_name: &str, f: &Field) -> Option<String> {
    if let Some(attr) = f.attrs.first() {
        if attr.path.segments.first().unwrap().ident == attr_name {
            match attr.parse_meta() {
                Ok(Meta::List(meta)) => {
                    for x in meta.nested {
                        match x {
                            NestedMeta::Meta(y) => {
                                match y {
                                    Meta::Path(_) => {}
                                    Meta::List(_) => {}
                                    Meta::NameValue(pair) => {
                                        let lit = pair.lit;
                                        match lit {
                                            Lit::Str(partial_name) => {
                                                return Some(partial_name.value());
                                            }
                                            _ => {}
                                        }
                                    }
                                }
                            }
                            NestedMeta::Lit(_) => {}
                        }
                    }
                },
                _ => {
                }
            }
        }
    }
    None
}

#[proc_macro_attribute]
pub fn sl_sh_fn(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut code: Vec<TokenStream2> = vec![];
    let mut item = match syn::parse::<syn::Item>(input) {
        Ok(item) => {
            item
        }
        _ => {
            let _e = BuilderError::new("No".to_string());
            unimplemented!();
        }
    };
    let fn_item: &mut ItemFn = match &mut item {
        syn::Item::Fn(fn_item) => fn_item,
        _ => panic!("Only works on functions!")
    };
    let sig_ident = &fn_item.sig.ident;
    let name = sig_ident.to_string();
    let builtin_name = "builtin_".to_string() + &name;
    let builtin_name = Ident::new(&builtin_name, Span::call_site());
    fn_item.block.stmts.insert(0,syn::parse(quote!(println!("hello world!");).into()).unwrap());
    let origin_fn_name = Ident::new(&name, Span::call_site());
    // keep original function
    //let len = fn_item.sig.inputs.len().to_string();
    //let len = Ident::new(&len, Span::call_site());
    code.push(item.into_token_stream().into());
    // add builtin that accepts sl-sh style arguments
    let tokens = quote! {
        use std::convert::TryInto;
        use std::convert::TryFrom;
        #(#code)*
        // fn builtin_int_to_float(
        //    environment: &mut Environment,
        //args: &mut dyn Iterator<Item = Expression>,
        fn #builtin_name(arg: sl_sh::ExpEnum) -> sl_sh::LispResult<sl_sh::types::Expression> {
            let result = #origin_fn_name(arg.try_into()?);
            let result: ExpEnum = result.into();
            Ok(result.into())
        }
    };
    TokenStream::from(tokens)
}
