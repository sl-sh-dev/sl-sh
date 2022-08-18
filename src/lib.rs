use quote::quote;
use quote::ToTokens;
use quote::__private::TokenStream;
use std::ops::Deref;
use syn::__private::{Span, TokenStream2};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, FnArg, Ident, Lit, Meta, NestedMeta,
    PathArguments, PathSegment, ReturnType, Type, TypePath,
};
extern crate static_assertions;

type MacroResult<T> = Result<T, syn::Error>;

const POSSIBLE_RESULT_TYPES: [&str; 1] = ["LispResult"];
const POSSIBLE_ARG_TYPES: [&str; 2] = ["Option", "Vec"];

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
        path: syn::Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

/// return the function names the macro will create. Given a base name, "base"
/// return (base, builtin_base, arg_parse_base) tuple of Idents
fn get_fn_names(original_item_fn: &syn::ItemFn) -> (Ident, Ident, Ident, Ident) {
    let sig_ident = &original_item_fn.sig.ident;
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
/// and crate::Expression types for generating the builtin function signature.
/// This always amounts to something like:
/// builtin_<fn-name>(arg_0: crate::Expression, arg_1: crate::Expression, ...)
/// The point is, this generated builtin function should have the exact same
/// number of arguments as the function we're wrapping. In the future if var args
/// or optionals are supported that can be portrayed on the Rust side as Optional<T>'s
/// for any optional arguments, and Vec<T>'s for any var args.
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
    let generic_argument = syn::GenericArgument::Type(ty);
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
        path: syn::Path {
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
    let generic_argument = syn::GenericArgument::Type(ty);
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
        path: syn::Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

/// parse the return type of the rust function
fn get_return_type(original_item_fn: &syn::ItemFn) -> MacroResult<(Type, Option<&'static str>)> {
    let return_type = match &original_item_fn.sig.output {
        ReturnType::Default => {
            unimplemented!("Functions with attribute must return a value.");
        }
        ReturnType::Type(_ra_arrow, ty) => *ty.clone(),
    };

    if let Some((inner_type, type_path)) = get_generic_argument_from_type(&return_type) {
        let wrapper = is_valid_generic_type(type_path, POSSIBLE_RESULT_TYPES.as_slice())?;
        match inner_type {
            syn::GenericArgument::Type(ty) => Ok((ty.clone(), Some(wrapper))),
            _ => {
                return Err(syn::Error::new(
                    original_item_fn.span(),
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
fn get_documentation_for_fn(original_item_fn: &syn::ItemFn) -> MacroResult<String> {
    let mut docs = "".to_string();
    for attr in &original_item_fn.attrs {
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
            original_item_fn.span(),
            "Functions with this attribute included must have documentation.",
        ))
    } else {
        Ok(docs)
    }
}

fn is_valid_generic_type<'a>(
    type_path: &TypePath,
    possible_types: &'a [&str],
) -> MacroResult<&'a str> {
    if type_path.path.segments.len() == 1 && type_path.path.segments.first().is_some() {
        let path_segment = &type_path.path.segments.first().unwrap();
        let ident = &path_segment.ident;
        for type_name in possible_types {
            if ident == type_name {
                return Ok(type_name);
            }
        }
    }
    Err(syn::Error::new(
            type_path.span(),
            format!(
                "Functions of with generic arguments of type {:?} must contain Types, see syn::GenericArgument.",
                possible_types
            ),
        ))
}

fn get_generic_argument_from_type_path(
    type_path: &syn::TypePath,
) -> Option<(&syn::GenericArgument, &syn::TypePath)> {
    if type_path.path.segments.len() == 1 {
        let path_segment = &type_path.path.segments.first()?;
        if let PathArguments::AngleBracketed(args) = &path_segment.arguments {
            if args.args.len() == 1 {
                let ty = args.args.first()?;
                return Some((ty, type_path));
            }
        }
    }
    None
}

fn get_generic_argument_from_type(
    ty: &syn::Type,
) -> Option<(&syn::GenericArgument, &syn::TypePath)> {
    if let Type::Path(ref type_path) = ty {
        get_generic_argument_from_type_path(type_path)
    } else {
        None
    }
}

fn generate_assertions_code_for_type_conversions(
    original_item_fn: &syn::ItemFn,
    return_type: &syn::Type,
) -> MacroResult<Vec<TokenStream2>> {
    let inputs = &original_item_fn.sig.inputs;
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
    original_item_fn: &syn::ItemFn,
    key: &str,
    values: &[(String, String)],
) -> MacroResult<Option<String>> {
    if values.is_empty() {
        Err(syn::Error::new(
            original_item_fn.span(),
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
    original_item_fn: &syn::ItemFn,
    fn_name: String,
    fn_name_attr: syn::Ident,
    builtin_name: syn::Ident,
    original_fn_name: syn::Ident,
) -> MacroResult<TokenStream> {
    let (return_type, wrapper) = get_return_type(original_item_fn)?;
    let conversions_assertions_code =
        generate_assertions_code_for_type_conversions(original_item_fn, &return_type)?;
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
    original_item_fn: &syn::ItemFn,
    attr_args: syn::AttributeArgs,
) -> MacroResult<(String, Ident, bool)> {
    let vals = attr_args
        .iter()
        .map(get_attribute_name_value)
        .collect::<MacroResult<Vec<(String, String)>>>()?;
    let fn_name_attr = "fn_name".to_string();
    let fn_name = get_attribute_value_with_key(original_item_fn, &fn_name_attr, vals.as_slice())?
        .ok_or_else(|| {
        syn::Error::new(
            original_item_fn.span(),
            "sl_sh_fn requires name-value pair, 'fn_name'",
        )
    })?;
    let fn_name_attr = Ident::new(&fn_name_attr, Span::call_site());

    let eval_values = if let Some(value) =
        get_attribute_value_with_key(original_item_fn, "eval_values", vals.as_slice())?
    {
        value == "true"
    } else {
        false
    };
    Ok((fn_name, fn_name_attr, eval_values))
}

fn generate_sl_sh_fns(
    original_item_fn: &syn::ItemFn,
    attr_args: syn::AttributeArgs,
) -> MacroResult<TokenStream2> {
    let (fn_name, fn_name_attr, eval_values) = parse_attributes(original_item_fn, attr_args)?;
    let doc_comments = get_documentation_for_fn(original_item_fn)?;

    let args_len = original_item_fn.sig.inputs.len();
    let (original_fn_name, builtin_name, parse_name, intern_name) = get_fn_names(original_item_fn);
    let builtins_fn_code = generate_builtin_fn(
        args_len,
        original_item_fn,
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

#[derive(Copy, Clone, Debug, PartialEq)]
enum ArgVal {
    Value,
    Optional,
    Vec,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum ArgPassingStyle {
    Move,
    Reference,    //(Option<syn::Lifetime>),
    MutReference, //(Option<syn::Lifetime>),
}

#[derive(Copy, Clone, Debug, PartialEq)]
struct Arg {
    val: ArgVal,
    passing_style: ArgPassingStyle,
}

fn get_arg_val(type_path: &syn::TypePath) -> MacroResult<ArgVal> {
    if let Some((_generic, type_path)) = get_generic_argument_from_type_path(type_path) {
        let wrapper = is_valid_generic_type(&type_path, POSSIBLE_ARG_TYPES.as_slice())?;
        if wrapper == "Option" {
            Ok(ArgVal::Optional)
        } else if wrapper == "Vec" {
            Ok(ArgVal::Vec)
        } else {
            return Err(syn::Error::new(
                type_path.span(),
                "Received generic argument this macro is not programmed to handle!",
            ));
        }
    } else {
        Ok(ArgVal::Value)
    }
}

fn parse_src_function_arguments(original_item_fn: &syn::ItemFn) -> MacroResult<Vec<Arg>> {
    let mut parsed_args = vec![];
    for fn_arg in original_item_fn.sig.inputs.iter() {
        match fn_arg {
            FnArg::Receiver(_) => {
                return Err(syn::Error::new(
                    original_item_fn.span(),
                    "Associated functions that take the self argument are not supported.",
                ))
            }
            FnArg::Typed(ty) => match &*ty.ty {
                Type::Path(ty) => {
                    let val = get_arg_val(ty)?;
                    parsed_args.push(Arg {
                        val,
                        passing_style: ArgPassingStyle::Move,
                    });
                }
                Type::Reference(ty_ref) => {
                    match &*ty_ref.elem {
                        Type::Path(ty) => {
                            let val = get_arg_val(ty)?;
                            //let lifetime = ty_ref.lifetime.clone();
                            let passing_style = if ty_ref.mutability.is_some() {
                                //ArgPassingStyle::MutReference(lifetime)
                                ArgPassingStyle::MutReference
                            } else {
                                //ArgPassingStyle::Reference(lifetime)
                                ArgPassingStyle::Reference
                            };
                            parsed_args.push(Arg { val, passing_style });
                        }
                        _ => {
                            return Err(syn::Error::new(
                                original_item_fn.span(),
                                &format!(
                                    "Only references/arguments of type path and reference are supported.: {:?})), ",
                                    ty.to_token_stream()
                                ),
                            ))
                        }
                    }
                }
                _ => {
                    return Err(syn::Error::new(
                        original_item_fn.span(),
                        &format!(
                            "Only references/arguments of type path and reference are supported.: {:?})), ",
                            ty.to_token_stream()
                        ),
                    ))
                }
            },
        }
    }
    Ok(parsed_args)
}

fn are_args_valid(original_item_fn: &syn::ItemFn, args: &[Arg]) -> MacroResult<()> {
    if args.is_empty() || args.len() == 1 {
        Ok(())
    } else {
        let mut found_opt = false;
        let mut found_value = false;
        for (i, arg) in args.iter().rev().enumerate() {
            match (i, arg.val, found_opt, found_value) {
                (i, ArgVal::Vec, _, _) if i > 0 => {
                    return Err(syn::Error::new(
                        original_item_fn.span(),
                        "Only one Vec argument is supported and it must be the last argument.",
                    ));
                }
                (_, ArgVal::Optional, _, true) => {
                    return Err(syn::Error::new(
                        original_item_fn.span(),
                        "Optional argument(s) must be placed last.",
                    ));
                }
                (_, ArgVal::Optional, _, _) => {
                    found_opt = true;
                }
                (_, ArgVal::Value, _, _) => {
                    found_value = true;
                }
                (_, _, _, _) => {}
            }
        }
        Ok(())
    }
}

fn generate_sl_sh_fn2(original_item_fn: &syn::ItemFn) -> MacroResult<TokenStream2> {
    let args = parse_src_function_arguments(original_item_fn)?;
    are_args_valid(original_item_fn, args.as_slice())?;
    Ok(quote!())
}

#[proc_macro_attribute]
pub fn sl_sh_fn2(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let _attr_args = parse_macro_input!(attr as syn::AttributeArgs);

    let tokens = match syn::parse::<syn::Item>(input) {
        Ok(item) => match &item {
            syn::Item::Fn(original_item_fn) => {
                let generated_code = match generate_sl_sh_fn2(original_item_fn) {
                    Ok(generated_code) => generated_code,
                    Err(e) => e.to_compile_error(),
                };
                let original_fn_code = item.into_token_stream();
                quote! {
                    #generated_code

                    #original_fn_code
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

#[proc_macro_attribute]
pub fn sl_sh_fn(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr_args = parse_macro_input!(attr as syn::AttributeArgs);

    let tokens = match syn::parse::<syn::Item>(input) {
        Ok(item) => match &item {
            syn::Item::Fn(original_item_fn) => {
                let generated_sl_sh_fns: TokenStream2 =
                    match generate_sl_sh_fns(original_item_fn, attr_args) {
                        Ok(generated_code) => generated_code,
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

#[cfg(test)]
mod test {
    use super::*;

    fn are_args_valid(args: &[Arg]) -> bool {
        if args.is_empty() || args.len() == 1 {
            true
        } else {
            let mut found_opt = false;
            let mut found_value = false;
            for (i, arg) in args.iter().rev().enumerate() {
                match (i, arg.val, found_opt, found_value) {
                    (i, ArgVal::Vec, _, _) if i > 0 => {
                        // vec can only be last argument
                        return false;
                    }
                    (_, ArgVal::Optional, _, true) => {
                        // optionals all must be last
                        return false;
                    }
                    (_, ArgVal::Optional, _, _) => {
                        found_opt = true;
                    }
                    (_, ArgVal::Value, _, _) => {
                        found_value = true;
                    }
                    (_, _, _, _) => {}
                }
            }
            true
        }
    }

    #[test]
    fn test_args() {
        let args = vec![];
        assert!(are_args_valid(args.as_slice()));
        // values are always valid
        let args = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::MutReference,
            },
        ];
        assert!(are_args_valid(args.as_slice()));

        // vec must be last argument
        let args = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::Move,
            },
        ];
        assert!(are_args_valid(args.as_slice()));

        // vec must be last argument
        let args = vec![
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
        ];
        assert!(!are_args_valid(args.as_slice()));

        // opt must be last argument
        let args = vec![
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
        ];
        assert!(!are_args_valid(args.as_slice()));

        // opt must be last argument(s)
        let args = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
        ];
        assert!(are_args_valid(args.as_slice()));

        // opt must be last argument(s), unless it's one vec in the last slot
        let args = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::Move,
            },
        ];
        assert!(are_args_valid(args.as_slice()));

        // vec must always be last
        let args = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Vec,
                passing_style: ArgPassingStyle::Reference,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
        ];
        assert!(!are_args_valid(args.as_slice()));

        // opt must always be last argument(s)
        let args = vec![
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Reference,
            },
        ];
        assert!(!are_args_valid(args.as_slice()));

        // opt must always be last argument(s)
        let args = vec![
            Arg {
                val: ArgVal::Optional,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Move,
            },
            Arg {
                val: ArgVal::Value,
                passing_style: ArgPassingStyle::Reference,
            },
        ];
        assert!(!are_args_valid(args.as_slice()));
    }
}

//TODO
//  - functions that return optional.
//  - functions that return values.
//  - support Option-al arguments... enhance get_input_types
//      - builtins_file.rs
//          + builtin_get_temp_file
//          + builtin_with_temp_dir
//      - builtins_hashmap.rs
//          + builtin_hash_get
//  - variadic functions
//      - builtins.rs
//          + builtin_apply
//          + builtin_unwind_protect
//      - builtins_types.rs
//          + builtin_to_symbol
//  - support functions that need futher manipulation with Environment... make make a version
//  of TryIntoExpression that takes environment?
//      - builtins_hashmap.rs
//          + builtin_make_hash
//          + builtin_hash_set
// - macro needs to know if a function turns a LispResult<()> so it can know to return Ok(())
// - should all macros just use the try_inner_exp_enum pattern?
// - what about a ToString coercion for ExpEnum::Char/String/Symbol for rust functions that require them
//  to turn into Strings.
// - given i only need to implement TryInto and TryIntoExpression
