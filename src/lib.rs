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
        BuilderError { details: msg }
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
            FnArg::Receiver(_) => {
                unimplemented!("FnArg::Receiver is not yet implemented.")
            }
            FnArg::Typed(ty) => {
                types.push(ty.ty.deref().clone());
            }
        }
    }
    types
}

fn build_sl_sh_exp_enum_type() -> Type {
    let crate_path_segment = PathSegment {
        ident: Ident::new("crate", Span::call_site()),
        arguments: PathArguments::None,
    };
    let exp_enum_path_segment = PathSegment {
        ident: Ident::new("ExpEnum", Span::call_site()),
        arguments: PathArguments::None,
    };
    let mut pun_seq = Punctuated::new();
    pun_seq.push(crate_path_segment);
    pun_seq.push(exp_enum_path_segment);
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

/// return the function names the macro will create. Given a base name, "base"
/// return (base, builtin_base, arg_parse_base) tuple of Idents
fn get_fn_names(fn_item: &ItemFn) -> (Ident, Ident, Ident) {
    let sig_ident = &fn_item.sig.ident;
    let name = sig_ident.to_string();
    let original_fn_name = Ident::new(&name, Span::call_site());
    let builtin_name = "builtin_".to_string() + &name;
    let builtin_name = Ident::new(&builtin_name, Span::call_site());
    let parse_name = "parse_".to_string() + &name;
    let parse_name = Ident::new(&parse_name, Span::call_site());
    (original_fn_name, builtin_name, parse_name)
}

/// given the length of the rust native args list, create two lists of argument names
/// and crate::ExpEnum types for generating the builtin function signature.
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

/// given a type and the string value of a trait in std::convert::<convert_trait>
/// returned the given type wrapped with the std::convert::<convert_trait>
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
    let return_type = get_return_type(fn_item);
    let to_return_type = wrap_with_std_convert(build_sl_sh_exp_enum_type(), "Into");
    conversion_assertions_code.push(quote! {
      static_assertions::assert_impl_all!(#return_type: #to_return_type);
    });
    conversion_assertions_code
}

fn get_attribute_value_with_key(key: &str, values: &[(String, String)]) -> Option<String> {
    let pair = values.iter().filter(|k| k.0 == key).take(1).next();
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
                        Some((ident.to_string(), partial_name.value()))
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
        .filter_map(get_attribute_name_pair)
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

    let conversions_assertions_code = generate_assertions_code_for_type_conversions(fn_item);

    let args_len = fn_item.sig.inputs.len();
    let (fn_args, fn_types) = generate_builtin_arg_list(args_len);
    let (original_fn_name, builtin_name, parse_name) = get_fn_names(fn_item);
    let original_fn_code = item.into_token_stream();

    let tokens = quote! {
        trait ExpandVecToArgs<Args> {
            type Output;
            fn call_expand_args(&self, args: Args) -> Self::Output;
        }

        impl<F, T, R> ExpandVecToArgs<[T; 0]> for F
        where
            F: Fn() -> R,
        {
            type Output = R;
            fn call_expand_args(&self, _args: [T; 0]) -> R {
                self()
            }
        }

        impl<F, T, R> ExpandVecToArgs<[T; 1]> for F
        where
            F: Fn(T) -> R,
        {
            type Output = R;
            fn call_expand_args(&self, args: [T; 1]) -> R {
                let [arg0] = args;
                self(arg0)
            }
        }

        impl<F, T, R> ExpandVecToArgs<[T; 2]> for F
        where
            F: Fn(T, T) -> R,
        {
            type Output = R;
            fn call_expand_args(&self, args: [T; 2]) -> R {
                let [arg0, arg1] = args;
                self(arg0, arg1)
            }
        }

        #original_fn_code

        use std::convert::TryInto;
        use std::convert::TryFrom;
        fn #builtin_name(#(#fn_args: #fn_types),*) -> crate::LispResult<crate::types::Expression> {
            #(#conversions_assertions_code)*
            // need the fn_name in the error in the try_into calls... these MUST have that metadata
            // because that's where we're offloading our error checking.
            let result = #original_fn_name(#(#fn_args.try_into()?),*);
            let result: ExpEnum = result.into();
            Ok(result.into())
        }

        fn #parse_name(
            environment: &mut crate::environment::Environment,
            args: &mut dyn Iterator<Item = crate::types::Expression>,
        ) -> crate::LispResult<crate::types::Expression> {
            let args = crate::builtins_util::make_args_exp_enums(environment, args)?;
            let #fn_name_attr = #fn_name;
            const args_len: usize = #args_len;
            if args.len() == args_len {
                let params: [crate::types::ExpEnum; args_len] = args.try_into().expect("sl_sh_fn proc_macro_attribute has incorrect information about arity of function it decorates.");
                #builtin_name.call_expand_args(params)
            } else if args.len() > args_len {
                Err(LispError::new(format!("{} given too many arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            } else {
                Err(LispError::new(format!("{} not given enough arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            }
        }
    };
    TokenStream::from(tokens)
}

//TODO
//  - functions that do not return anything
//  - functions that take actual expenums, s.t. doing into on them might... be redundant?
//  - variadic functions
