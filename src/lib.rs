use proc_macro::TokenStream;
use quote::quote;
use quote::ToTokens;
use std::error::Error;
use std::fmt;
use std::ops::Deref;
use syn::__private::{Span, TokenStream2};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, AttributeArgs, FnArg, GenericArgument,
    Ident, ItemFn, Lit, Meta, NestedMeta, Path, PathArguments, PathSegment, ReturnType, Type,
    TypePath,
};
extern crate static_assertions;

#[derive(Debug)]
struct BuilderError {
    details: String,
}

impl BuilderError {
    fn new(msg: String) -> BuilderError {
        BuilderError {
            details: msg.to_string(),
        }
    }
}

//TODO how to use BuilderError properly over unimplemented calls.
impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.details)
    }
}

impl Error for BuilderError {
    fn description(&self) -> &str {
        &self.details
    }
}

fn get_input_types(inputs: &Punctuated<FnArg, Comma>) -> Vec<Type> {
    let mut types = vec![];
    for input in inputs {
        match input {
            FnArg::Receiver(_) => {}
            FnArg::Typed(ty) => {
                types.push(ty.ty.deref().clone());
            }
        }
    }
    types
}

fn build_sl_sh_exp_enum_type() -> Type {
    let sl_sh_path_segment = PathSegment {
        ident: Ident::new("sl_sh", Span::call_site()),
        arguments: PathArguments::None,
    };
    let exp_enum_path_segment = PathSegment {
        ident: Ident::new("ExpEnum", Span::call_site()),
        arguments: PathArguments::None,
    };
    let mut pun_seq = Punctuated::new();
    pun_seq.push(sl_sh_path_segment);
    pun_seq.push(exp_enum_path_segment);
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

fn get_fn_names(fn_item: &ItemFn) -> (Ident, Ident) {
    let sig_ident = &fn_item.sig.ident;
    let name = sig_ident.to_string();
    let builtin_name = "builtin_".to_string() + &name;
    let builtin_name = Ident::new(&builtin_name, Span::call_site());
    let original_fn_name = Ident::new(&name, Span::call_site());
    (builtin_name, original_fn_name)
}

fn generate_builtin_arg_list(len: usize) -> (Vec<Ident>, Vec<Type>) {
    let mut fn_args = vec![];
    let mut fn_types = vec![];
    for i in 0..len {
        let arg_name = "arg_".to_string() + &i.to_string();
        let arg = Ident::new(&arg_name, Span::call_site());
        fn_args.push(arg);
        let ty = build_sl_sh_exp_enum_type();
        fn_types.push(ty);
    }
    (fn_args, fn_types)
}

fn wrap_with_std_convert(ty: Type, convert_trait: &str) -> Type {
    let std_path_segment = PathSegment {
        ident: Ident::new("std", Span::call_site()),
        arguments: PathArguments::None,
    };
    let convert_path_segment = PathSegment {
        ident: Ident::new("convert", Span::call_site()),
        arguments: PathArguments::None,
    };
    let generic_argument = GenericArgument::Type(ty);
    let mut generic_pun_seq = Punctuated::new();
    generic_pun_seq.push(generic_argument);
    let generic_argument = AngleBracketedGenericArguments {
        colon2_token: None,
        lt_token: Default::default(),
        args: generic_pun_seq,
        gt_token: Default::default(),
    };
    let trait_path_segment = PathSegment {
        ident: Ident::new(convert_trait, Span::call_site()),
        arguments: PathArguments::AngleBracketed(generic_argument),
    };
    let mut pun_seq = Punctuated::new();
    pun_seq.push(std_path_segment);
    pun_seq.push(convert_path_segment);
    pun_seq.push(trait_path_segment);
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

fn get_return_type(fn_item: &ItemFn) -> Type {
    let return_type = match &fn_item.sig.output {
        ReturnType::Default => {
            unimplemented!("Functions with attribute must return a value.");
        }
        ReturnType::Type(_ra_arrow, ty) => *ty.clone(),
    };
    return_type
}

fn generate_assertions_code_for_type_conversions(fn_item: &ItemFn) -> Vec<TokenStream2> {
    let inputs = &fn_item.sig.inputs;
    let input_types = get_input_types(inputs);
    let mut conversion_assertions_code = vec![];
    for input_type in input_types {
        let try_into = wrap_with_std_convert(input_type, "TryInto");
        let exp_enum = build_sl_sh_exp_enum_type();
        conversion_assertions_code.push(quote! {
          static_assertions::assert_impl_all!(#exp_enum: #try_into);
        });
    }
    let return_type = get_return_type(&fn_item);
    let to_return_type = wrap_with_std_convert(build_sl_sh_exp_enum_type(), "Into");
    conversion_assertions_code.push(quote! {
      static_assertions::assert_impl_all!(#return_type: #to_return_type);
    });
    conversion_assertions_code
}

fn get_attribute_value_with_key(key: &str, values: &[(String, String)]) -> Option<String> {
    let pair = values.iter().filter(|k| &k.0 == key).take(1).next();
    pair.map(|pair| pair.1.to_string())
}

fn get_attribute_name_pair(nested_meta: &NestedMeta) -> Option<(String, String)> {
    match nested_meta {
        NestedMeta::Meta(meta) => match meta {
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
        },
        NestedMeta::Lit(_) => {
            unimplemented!("2 Only support attributes of form (name = \"value\")");
        }
    }
}

#[proc_macro_attribute]
pub fn sl_sh_fn(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr_args = parse_macro_input!(attr as AttributeArgs);
    let vals = attr_args
        .iter()
        .map(get_attribute_name_pair)
        .filter_map(|val| val)
        .collect::<Vec<(String, String)>>();
    let fn_name_attr = "fn_name".to_string();
    let fn_name = get_attribute_value_with_key(&fn_name_attr, &vals)
        .expect("Attribute 'fn_name' name-value pair must be set.");
    let fn_name_attr = Ident::new(&fn_name_attr, Span::call_site());

    let mut item = match syn::parse::<syn::Item>(input) {
        Ok(item) => item,
        _ => {
            let _e = BuilderError::new("No".to_string());
            unimplemented!();
        }
    };
    let fn_item: &mut ItemFn = match &mut item {
        syn::Item::Fn(fn_item) => fn_item,
        _ => unimplemented!("sl_sh_fn proc_macro_attribute only works on functions."),
    };

    let conversions_assertions_code = generate_assertions_code_for_type_conversions(&fn_item);

    let (fn_args, fn_types) = generate_builtin_arg_list(fn_item.sig.inputs.len());
    let (builtin_name, original_fn_name) = get_fn_names(&fn_item);
    let original_fn_code = item.into_token_stream();

    let tokens = quote! {
        #original_fn_code

        use std::convert::TryInto;
        use std::convert::TryFrom;
        fn #builtin_name(#(#fn_args: #fn_types),*) -> sl_sh::LispResult<sl_sh::types::Expression> {
            #(#conversions_assertions_code)*
            let result = #original_fn_name(#(#fn_args.try_into()?),*);
            let result: ExpEnum = result.into();
            let #fn_name_attr = #fn_name;
            Ok(result.into())
        }
    };
    TokenStream::from(tokens)
}

//TODO
//  - functions that do not return anything
//  - functions that take actual expenums, s.t. doing into on them might... be redundant?
