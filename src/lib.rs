use quote::quote;
use quote::ToTokens;
use quote::__private::TokenStream;
use std::ops::Deref;
use syn::__private::{Span, TokenStream2};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, FnArg, GenericArgument, Ident, Lit, Meta,
    NestedMeta, Path, PathArguments, PathSegment, ReturnType, Type, TypePath,
};
extern crate static_assertions;

type MacroResult<T> = Result<T, syn::Error>;

const POSSIBLE_RESULT_TYPES: [&str; 1] = ["LispResult"];

/// parse and return the rust types of the function parameters
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

/// return a fully qualified crate::Expression Type this is the struct that
/// all sl_sh types are wrapped in, because it's a lisp.
fn build_sl_sh_expression_type() -> Type {
    let crate_path_segment = PathSegment {
        ident: Ident::new("crate", Span::call_site()),
        arguments: PathArguments::None,
    };
    let exp_enum_path_segment = PathSegment {
        ident: Ident::new("Expression", Span::call_site()),
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
fn get_fn_names(item_fn: &syn::ItemFn) -> (Ident, Ident, Ident, Ident) {
    let sig_ident = &item_fn.sig.ident;
    let name = sig_ident.to_string();
    let original_fn_name = Ident::new(&name, Span::call_site());
    let builtin_name = "builtin_".to_string() + &name;
    let builtin_name = Ident::new(&builtin_name, Span::call_site());
    let parse_name = "parse_".to_string() + &name;
    let parse_name = Ident::new(&parse_name, Span::call_site());
    let intern_name = "intern_".to_string() + &name;
    let intern_name = Ident::new(&intern_name, Span::call_site());
    (original_fn_name, builtin_name, parse_name, intern_name)
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
        let ty = build_sl_sh_expression_type();
        fn_types.push(ty);
    }
    (fn_args, fn_types)
}

/// given a type and the string value of a trait in std::convert::<convert_trait>
/// returned the given type wrapped with the std::convert::<convert_trait>
fn wrap_with_try_into_expression(ty: Type) -> Type {
    let crate_path_segment = PathSegment {
        ident: Ident::new("crate", Span::call_site()),
        arguments: PathArguments::None,
    };
    let builtins_util_path_segment = PathSegment {
        ident: Ident::new("builtins_util", Span::call_site()),
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
    let try_into_expression_path_segment = PathSegment {
        ident: Ident::new("TryIntoExpression", Span::call_site()),
        arguments: PathArguments::AngleBracketed(generic_argument),
    };
    let mut pun_seq = Punctuated::new();
    pun_seq.push(crate_path_segment);
    pun_seq.push(builtins_util_path_segment);
    pun_seq.push(try_into_expression_path_segment);
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
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

/// parse the return type of the rust function
fn get_return_type(item_fn: &syn::ItemFn) -> MacroResult<(Type, Option<&'static str>)> {
    let return_type = match &item_fn.sig.output {
        ReturnType::Default => {
            unimplemented!("Functions with attribute must return a value.");
        }
        ReturnType::Type(_ra_arrow, ty) => *ty.clone(),
    };

    if let Some(inner_type) = get_inner_type(&return_type) {
        let wrapper = is_valid_inner_type(&return_type)?;
        match inner_type {
            GenericArgument::Type(ty) => Ok((ty.clone(), Some(wrapper))),
            _ => {
                return Err(syn::Error::new(
                    item_fn.span(),
                    format!(
                        "Functions of with generic arguments of type {:?} must contain Types, see syn::GenericArgument.",
                        &POSSIBLE_RESULT_TYPES
                    ),
                ))
            }
        }
    } else {
        Ok((return_type, None))
    }
}

/// Pull out every #doc attribute on the target fn for the proc macro attribute.
/// Ignore any other attributes and only Err if there are no #doc attributes.
fn get_documentation_for_fn(item_fn: &syn::ItemFn) -> MacroResult<String> {
    let mut docs = "".to_string();
    for attr in &item_fn.attrs {
        for path_segment in attr.path.segments.iter() {
            if &path_segment.ident.to_string() == "doc" {
                if let Ok(Meta::NameValue(pair)) = attr.parse_meta() {
                    if let Lit::Str(partial_name) = &pair.lit {
                        docs += &*partial_name.value();
                        docs += "\n";
                    }
                }
            }
        }
    }
    if docs.is_empty() {
        Err(syn::Error::new(
            item_fn.span(),
            "Functions with this attribute included must have documentation.",
        ))
    } else {
        Ok(docs)
    }
}

fn is_valid_inner_type(ty: &Type) -> MacroResult<&'static str> {
    return if let Type::Path(ref type_path) = ty {
        if type_path.path.segments.len() == 1 && type_path.path.segments.first().is_some() {
            let path_segment = &type_path.path.segments.first().unwrap();
            let ident = &path_segment.ident;
            for type_name in &POSSIBLE_RESULT_TYPES {
                if ident == type_name {
                    return Ok(type_name);
                }
            }
        }
        Err(syn::Error::new(
            ty.span(),
            format!(
                "Functions of with generic arguments of type {:?} must contain Types, see syn::GenericArgument.",
                &POSSIBLE_RESULT_TYPES
            ),
        ))
    } else {
        Err(syn::Error::new(
            ty.span(),
            format!(
                "Expected type with one of generic arguments type {:?}.",
                &POSSIBLE_RESULT_TYPES
            ),
        ))
    };
}

fn get_inner_type(ty: &Type) -> Option<&GenericArgument> {
    if let Type::Path(ref type_path) = ty {
        if type_path.path.segments.len() == 1 {
            let path_segment = &type_path.path.segments.first()?;
            if let PathArguments::AngleBracketed(args) = &path_segment.arguments {
                if args.args.len() == 1 {
                    let ty = args.args.first()?;
                    return Some(ty);
                }
            }
        }
    }
    None
}

fn generate_assertions_code_for_type_conversions(
    item_fn: &syn::ItemFn,
    return_type: &syn::Type,
) -> MacroResult<Vec<TokenStream2>> {
    let inputs = &item_fn.sig.inputs;
    let input_types = get_input_types(inputs);
    let mut conversion_assertions_code = vec![];
    for input_type in input_types {
        let try_into = wrap_with_std_convert(input_type.clone(), "TryInto");
        let try_into_expression = wrap_with_try_into_expression(input_type);
        let expression = build_sl_sh_expression_type();
        conversion_assertions_code.push(quote! {
          static_assertions::assert_impl_all!(#expression: #try_into);
          static_assertions::assert_impl_all!(#expression: #try_into_expression);
        });
    }
    let to_return_type = wrap_with_std_convert(build_sl_sh_expression_type(), "Into");
    conversion_assertions_code.push(quote! {
      static_assertions::assert_impl_all!(#return_type: #to_return_type);
    });
    Ok(conversion_assertions_code)
}

fn get_attribute_value_with_key(
    item_fn: &syn::ItemFn,
    key: &str,
    values: &[(String, String)],
) -> MacroResult<Option<String>> {
    if values.is_empty() {
        Err(syn::Error::new(
            item_fn.span(),
            "sl_sh_fn requires at least one name-value pair, 'fn_name = \"<name-of-sl-sh-fun>\"'.",
        ))
    } else {
        for name_value in values {
            if name_value.0 == key {
                return Ok(Some(name_value.1.to_string()));
            }
        }
        Ok(None)
    }
}

fn get_attribute_name_value(nested_meta: &NestedMeta) -> MacroResult<(String, String)> {
    match nested_meta {
        NestedMeta::Meta(meta) => match meta {
            Meta::NameValue(pair) => {
                let path = &pair.path;
                let lit = &pair.lit;
                match (path.get_ident(), lit) {
                    (Some(ident), Lit::Str(partial_name)) => {
                        Ok((ident.to_string(), partial_name.value()))
                    }
                    (Some(ident), Lit::Bool(b)) => {
                        Ok((ident.to_string(), b.value.to_string()))
                    }
                    (_, _) => Err(syn::Error::new(
                        meta.span(),
                        "sl_sh_fn requires one name-value pair, 'fn_name'. Supports optional name-value pair 'eval_values = true')",
                    )),
                }
            }
            other => Err(syn::Error::new(
                other.span(),
                "sl_sh_fn only supports one name-value pair attribute argument, 'fn_name'.",
            )),
        },
        other => Err(syn::Error::new(
            other.span(),
            "sl_sh_fn only supports one name-value pair attribute argument, 'fn_name'.",
        )),
    }
}

fn generate_builtin_fn(
    args_len: usize,
    item_fn: &syn::ItemFn,
    fn_name: String,
    fn_name_attr: syn::Ident,
    builtin_name: syn::Ident,
    original_fn_name: syn::Ident,
) -> MacroResult<TokenStream> {
    let (return_type, wrapper) = get_return_type(item_fn)?;
    let conversions_assertions_code =
        generate_assertions_code_for_type_conversions(item_fn, &return_type)?;
    let (fn_args, fn_types) = generate_builtin_arg_list(args_len);
    let tokens = if let Some(_wrapper) = wrapper {
        quote! {
            fn #builtin_name(#(#fn_args: #fn_types),*) -> crate::LispResult<crate::types::Expression> {
                use std::convert::TryInto;
                use std::convert::Into;
                use crate::builtins_util::TryIntoExpression;
                let #fn_name_attr = #fn_name;
                #(#conversions_assertions_code)*
                let result = #original_fn_name(#(#fn_args.try_into_for(#fn_name_attr)?),*)?;
                Ok(result.into())
            }
        }
    } else {
        quote! {
            fn #builtin_name(#(#fn_args: #fn_types),*) -> crate::LispResult<crate::types::Expression> {
                use std::convert::TryInto;
                use std::convert::Into;
                use crate::builtins_util::TryIntoExpression;
                let #fn_name_attr = #fn_name;
                #(#conversions_assertions_code)*
                let result = #original_fn_name(#(#fn_args.try_into_for(#fn_name_attr)?),*);
                Ok(result.into())
            }
        }
    };
    Ok(tokens)
}

fn parse_attributes(
    item_fn: &syn::ItemFn,
    attr_args: syn::AttributeArgs,
) -> MacroResult<(String, Ident, bool)> {
    let vals = attr_args
        .iter()
        .map(get_attribute_name_value)
        .collect::<MacroResult<Vec<(String, String)>>>()?;
    let fn_name_attr = "fn_name".to_string();
    let fn_name = get_attribute_value_with_key(item_fn, &fn_name_attr, vals.as_slice())?
        .ok_or_else(|| {
            syn::Error::new(
                item_fn.span(),
                "sl_sh_fn requires name-value pair, 'fn_name'",
            )
        })?;
    let fn_name_attr = Ident::new(&fn_name_attr, Span::call_site());

    let eval_values = if let Some(value) =
        get_attribute_value_with_key(item_fn, "eval_values", vals.as_slice())?
    {
        value == "true"
    } else {
        false
    };
    Ok((fn_name, fn_name_attr, eval_values))
}

fn generate_sl_sh_fns(
    item_fn: &syn::ItemFn,
    attr_args: syn::AttributeArgs,
) -> MacroResult<TokenStream2> {
    let (fn_name, fn_name_attr, eval_values) = parse_attributes(item_fn, attr_args)?;
    let doc_comments = get_documentation_for_fn(item_fn)?;

    let args_len = item_fn.sig.inputs.len();
    let (original_fn_name, builtin_name, parse_name, intern_name) = get_fn_names(item_fn);
    let builtins_fn_code = generate_builtin_fn(
        args_len,
        item_fn,
        fn_name.clone(),
        fn_name_attr.clone(),
        builtin_name.clone(),
        original_fn_name,
    )?;

    let make_args = if eval_values {
        quote! {
            let args = crate::builtins_util::make_args(environment, args)?;
        }
    } else {
        quote! {
            let args = crate::builtins_util::make_args_eval_no_values(environment, args)?;
        }
    };

    let tokens = quote! {
        #builtins_fn_code

        fn #parse_name(
            environment: &mut crate::environment::Environment,
            args: &mut dyn Iterator<Item = crate::types::Expression>,
        ) -> crate::LispResult<crate::types::Expression> {
            use std::convert::TryInto;
            use crate::builtins_util::ExpandVecToArgs;
            #make_args
            let #fn_name_attr = #fn_name;
            const args_len: usize = #args_len;
            if args.len() == args_len {
                match args.try_into() {
                    Ok(params) => {
                        let params: [crate::types::Expression; args_len] = params;
                        #builtin_name.call_expand_args(params)
                    },
                    Err(e) => {
                        Err(LispError::new(format!("{} is broken and can't parse its arguments..", #fn_name_attr, )))
                    }
                }
            } else if args.len() > args_len {
                Err(LispError::new(format!("{} given too many arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            } else {
                Err(LispError::new(format!("{} not given enough arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            }
        }

        fn #intern_name<S: std::hash::BuildHasher>(
            interner: &mut Interner,
            data: &mut std::collections::HashMap<&'static str, (crate::types::Expression, String), S>,
        ) {
            let #fn_name_attr = #fn_name;
            data.insert(
                interner.intern(#fn_name_attr),
                crate::types::Expression::make_function(#parse_name, #doc_comments),
            );
        }
    };
    Ok(tokens)
}

#[proc_macro_attribute]
pub fn sl_sh_fn(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr_args = parse_macro_input!(attr as syn::AttributeArgs);

    let tokens = match syn::parse::<syn::Item>(input) {
        Ok(item) => match &item {
            syn::Item::Fn(item_fn) => {
                let generated_sl_sh_fns: TokenStream2 = match generate_sl_sh_fns(item_fn, attr_args)
                {
                    Ok(fns) => fns,
                    Err(e) => e.to_compile_error(),
                };
                let original_fn_code = item.into_token_stream();
                quote! {
                    #original_fn_code

                    #generated_sl_sh_fns
                }
            }
            _ => syn::Error::new(item.span(), "This attribute only supports functions.")
                .to_compile_error(),
        },
        Err(e) => {
            syn::Error::new(e.span(), "Failed to parse proc_macro_attribute.").to_compile_error()
        }
    };

    proc_macro::TokenStream::from(tokens)
}

//TODO
//  - functions that do not return anything... enhance get_return_type
//  - support Option-al arguments... enhance get_input_types
//  - variadic functions
//  - support functions that return LispResult<T> by default, not just an primitive
