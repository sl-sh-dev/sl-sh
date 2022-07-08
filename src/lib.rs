use proc_macro::TokenStream;
use std::error::Error;
use quote::ToTokens;
use std::fmt;
use quote::{quote, quote_spanned};
use syn::{parse_macro_input, DeriveInput, Ident, Data, Fields, Type, PathArguments, Field, GenericArgument, Meta, NestedMeta, Lit, ItemFn, Path, Attribute, AttributeArgs};
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

fn get_attribute_name_pair(nested_meta: &NestedMeta) -> Option<(String, String)> {
    match nested_meta {
        NestedMeta::Meta(meta) => {
            match meta {
                Meta::NameValue(pair) => {
                    let path = &pair.path;
                    let lit = &pair.lit;
                    match (path.get_ident(), lit) {
                        (Some(ident), Lit::Str(partial_name)) => {
                            return Some((ident.to_string(), partial_name.value()));
                        }
                        (_, _) => {
                            unimplemented!("0 Only support attributes of form (name = \"value\")");
                        }
                    }
                }
                _ => {
                    unimplemented!("1 Only support attributes of form (name = \"value\")");
                }
            }
        }
        NestedMeta::Lit(_) => {
            unimplemented!("2 Only support attributes of form (name = \"value\")");
        }
    }
    None
}

#[proc_macro_attribute]
pub fn sl_sh_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(attr as AttributeArgs);
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
    let vals = attr_args.iter().map(get_attribute_name_pair).filter(|val| val.is_some()).map(|val| val.unwrap()).collect::<Vec<(String, String)>>();
    let len = vals.len();
    let len = "len_of_attrs_".to_string() + &len.to_string();
    let len = Ident::new(&len, Span::call_site());
    let first_pair = vals.get(0).unwrap();
    let pair0 = &first_pair.0;
    let pair1 = &first_pair.1;
    let pair0 = Ident::new(&pair0, Span::call_site());
    let pair1 = Ident::new(&pair1, Span::call_site());
    let sig_ident = &fn_item.sig.ident;
    let name = sig_ident.to_string();
    let builtin_name = "builtin_".to_string() + &name;
    let builtin_name = Ident::new(&builtin_name, Span::call_site());
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
            let #len = "";
            let #pair0 = "";
            let #pair1 = "";
            Ok(result.into())
        }
    };
    TokenStream::from(tokens)
}
