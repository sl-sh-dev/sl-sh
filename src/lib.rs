//TODO why am I using these __private versions, is it
// just to skip out on some annoying .into()'s on TokenStream2
use quote::quote;
use quote::ToTokens;
use quote::__private::TokenStream;
use std::ops::Deref;
use syn::__private::{Span, TokenStream2};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::token::Comma;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, FnArg, Ident, ItemFn, Lit, Meta, NestedMeta,
    PathArguments, PathSegment, ReturnType, Type,
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
    Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

/// return the function names the macro will create. Given a base name, "base"
/// return (base, builtin_base, arg_parse_base) tuple of Idents
fn get_fn_names(original_item_fn: &ItemFn) -> (Ident, Ident, Ident, Ident) {
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
    Type::Path(syn::TypePath {
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
    Type::Path(syn::TypePath {
        qself: None,
        path: syn::Path {
            leading_colon: None,
            segments: pun_seq,
        },
    })
}

/// returns the option of inner type and the wrapped generic type (None if it's
/// not generic. If there is no return type None, None is returned. Throws
/// an error if the generic return type is not in the list of predefined
/// constants POSSIBLE_RESULT_TYPES.
fn get_return_type2(
    original_item_fn: &ItemFn,
) -> MacroResult<(Option<Type>, Option<&'static str>)> {
    let return_type = match &original_item_fn.sig.output {
        ReturnType::Default => return Ok((None, None)),
        ReturnType::Type(_ra_arrow, ty) => *ty.clone(),
    };

    if let Some((inner_type, type_path)) = get_generic_argument_from_type(&return_type) {
        let wrapper = is_valid_generic_type(type_path, POSSIBLE_RESULT_TYPES.as_slice())?;
        match inner_type {
            //syn::GenericArgument::Type(ty) => Ok((Some(syn::Type::Path(type_path.clone())), Some(wrapper))),
            syn::GenericArgument::Type(ty) => Ok((Some(ty.clone()), Some(wrapper))),
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
        Ok((Some(return_type), None))
    }
}

/// parse the return type of the rust function
fn get_return_type(original_item_fn: &ItemFn) -> MacroResult<(Type, Option<&'static str>)> {
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
fn get_documentation_for_fn(original_item_fn: &ItemFn) -> MacroResult<String> {
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
    type_path: &syn::TypePath,
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
        for path_segment in &type_path.path.segments.iter().rev().next() {
            if let PathArguments::AngleBracketed(args) = &path_segment.arguments {
                if let Some(ty) = args.args.iter().rev().next() {
                    return Some((ty, type_path));
                }
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

fn generate_assertions_code_for_return_type_conversions(return_type: &syn::Type) -> TokenStream2 {
    let to_return_type = wrap_with_std_convert(build_sl_sh_expression_type(), "Into");
    quote! {
      static_assertions::assert_impl_all!(#return_type: #to_return_type);
    }
}

fn generate_assertions_code_for_type_conversions(
    original_item_fn: &ItemFn,
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
    conversion_assertions_code.push(generate_assertions_code_for_return_type_conversions(
        return_type,
    ));
    Ok(conversion_assertions_code)
}

fn get_attribute_value_with_key(
    original_item_fn: &ItemFn,
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
    original_item_fn: &ItemFn,
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
    original_item_fn: &ItemFn,
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
    original_item_fn: &ItemFn,
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
                        Err(crate::types::LispError::new(format!("{} is broken and can't parse its arguments..", #fn_name_attr, )))
                    }
                }
            } else if args.len() > args_len {
                Err(crate::types::LispError::new(format!("{} given too many arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            } else {
                Err(crate::types::LispError::new(format!("{} not given enough arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
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
    Reference,
    MutReference,
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
                "Received generic argument this macro is not programmed to handle, only support Option and Vec!",
            ));
        }
    } else {
        Ok(ArgVal::Value)
    }
}

fn parse_value(arg_name: &syn::Ident, inner: TokenStream) -> TokenStream {
    quote! {
        sl_sh::ret_err_exp_enum!(
                #arg_name,
                sl_sh::ArgType::Exp(#arg_name),
                #inner,
                "sl_sh_fn macro is broken. ArgType::Exp can't be parsed as ArgType::Exp"
            )
    }
}

fn parse_optional(arg_name: &syn::Ident, inner: TokenStream) -> TokenStream {
    quote! {
        sl_sh::ret_err_exp_enum!(
                #arg_name,
                sl_sh::ArgType::Opt(#arg_name),
                #inner,
                "sl_sh_fn macro is broken. ArgType::Opt can't be parsed as ArgType::Opt"
            )
    }
}

fn parse_varargs(arg_name: &syn::Ident, inner: TokenStream) -> TokenStream {
    quote! {
        sl_sh::ret_err_exp_enum!(
                #arg_name,
                sl_sh::ArgType::VarArgs(#arg_name),
                #inner,
                "sl_sh_fn macro is broken. ArgType::Vargs can't be parsed as ArgType::Vargs"
            )
    }
}

fn get_parser_for_arg_val(val: ArgVal) -> fn(&Ident, TokenStream) -> TokenStream {
    match val {
        ArgVal::Value => parse_value,
        ArgVal::Optional => parse_optional,
        ArgVal::Vec => parse_varargs,
    }
}

fn make_orig_fn_call(
    original_item_fn: &ItemFn,
    original_fn_name: &Ident,
    arg_names: Vec<Ident>,
) -> MacroResult<TokenStream> {
    // the original function call must return an Expression object
    // this means all returned rust native types must implement TryIntoExpression
    // this is nested inside the builtin expression which must always
    // return a LispResult.
    let (return_type, lisp_result) = get_return_type2(original_item_fn)?;
    let returns_none = "()" == return_type.to_token_stream().to_string();
    let original_fn_call = match (return_type.clone(), lisp_result, returns_none) {
        // coerce to a LispResult<Expression>
        (Some(_), Some(_), true) => quote! {
            #original_fn_name(#(#arg_names),*)?;
            return Ok(());
        },
        (Some(_), Some(_), false) => quote! {
            return #original_fn_name(#(#arg_names),*);
        },
        // coerce to Expression
        (Some(_), None, _) => quote! {
            return Ok(#original_fn_name(#(#arg_names),*).into());
        },
        (None, Some(_), _) => {
            unreachable!("If this functions returns a LispResult it must also return a value.");
        }
        // no return
        (None, None, _) => quote! {
            #original_fn_name(#(#arg_names),*);
            return Ok(())
        },
    };
    Ok(quote! {
        #original_fn_call
    })
}

fn make_arg_types(original_item_fn: &ItemFn) -> MacroResult<(Vec<Ident>, Vec<TokenStream>)> {
    let len = original_item_fn.sig.inputs.len();
    // TODO this code can be saved but all rust types that map to sl_sh types will need a marker trait
    //  otherwise, there's not a good way to check at compile time if the long ExpEnum match statement
    //  will work at runtime.
    //  marker traits could

    // TODO conversion for return type only.
    //let conversions_assertions_code =
    //    generate_assertions_code_for_type_conversions(original_item_fn, &return_type)?;
    let mut arg_names = vec![];
    let mut arg_types = vec![];
    for i in 0..len {
        let parse_name = "arg_".to_string() + &i.to_string();
        let parse_name = Ident::new(&parse_name, Span::call_site());
        arg_names.push(parse_name);
        arg_types.push(quote! { sl_sh::ArgType })
    }
    Ok((arg_names, arg_types))
}

fn generate_builtin_fn2(
    original_item_fn: &ItemFn,
    original_fn_name: &Ident,
    builtin_name: &Ident,
) -> MacroResult<TokenStream> {
    let (arg_names, arg_types) = make_arg_types(original_item_fn)?;
    let orig_fn_call = make_orig_fn_call(original_item_fn, original_fn_name, arg_names.clone())?;

    let mut prev_token_stream = orig_fn_call;
    let fn_args = original_item_fn.sig.inputs.iter().zip(arg_names.iter());
    for (fn_arg, ident) in fn_args {
        match fn_arg {
            FnArg::Typed(ty) => match &*ty.ty {
                Type::Path(ty) => {
                    let val = get_arg_val(ty)?;
                    let parse_layer_1 = get_parser_for_arg_val(val);
                    let passing_style = ArgPassingStyle::Move;
                    prev_token_stream = parse_type(
                        ty,
                        prev_token_stream.clone(),
                        val,
                        ident,
                        passing_style,
                        parse_layer_1,
                    )?;
                }
                Type::Reference(ty_ref) => match &*ty_ref.elem {
                    Type::Path(ty) => {
                        let val = get_arg_val(ty)?;
                        let parse_layer_1 = get_parser_for_arg_val(val);
                        let passing_style = if ty_ref.mutability.is_some() {
                            ArgPassingStyle::MutReference
                        } else {
                            ArgPassingStyle::Reference
                        };
                        prev_token_stream = parse_type(
                            ty,
                            prev_token_stream.clone(),
                            val,
                            ident,
                            passing_style,
                            parse_layer_1,
                        )?;
                    }
                    _ => {}
                },
                _ => {}
            },
            _ => {}
        }
    }
    let (return_type, lisp_result) = get_return_type2(original_item_fn)?;
    let returns_none = "()" == return_type.to_token_stream().to_string();
    let ret_tokens = match (return_type, lisp_result, returns_none) {
        (Some(_), Some(_), true) | (None, None, _) | (Some(_), None, true) => {
            quote! { -> sl_sh::LispResult<()> }
        }
        (Some(return_type), _, false) => {
            quote! { -> sl_sh::LispResult<#return_type> }
        }
        (None, _, _) => {
            unreachable!("If this functions returns a LispResult it must also return a value.");
        }
    };
    let tokens = quote! {
        fn #builtin_name(#(#arg_names: #arg_types),*) #ret_tokens {
            #prev_token_stream
        }
    };
    Ok(tokens)
}

/// return a tuple meant for the ret_err_exp_enum and try_exp_enum macros.
/// first tuple is how to access the exp_enum data (mutable or immutable)
/// second tuple is how to refer to the inner exp enum in a match pattern
fn tokens_for_matching_references(
    arg_name: &syn::Ident,
    passing_style: ArgPassingStyle,
) -> (TokenStream, TokenStream) {
    match passing_style {
        ArgPassingStyle::Move => (quote! {#arg_name.get().data}, quote! {#arg_name}),
        ArgPassingStyle::Reference => (quote! {#arg_name.get().data}, quote! {ref #arg_name}),
        ArgPassingStyle::MutReference => (
            quote! {#arg_name.get_mut().data},
            quote! {ref mut #arg_name},
        ),
    }
}

fn parse_argval_varargs_type(
    ty: &syn::TypePath,
    arg_name: &Ident,
    passing_style: ArgPassingStyle,
    inner: TokenStream,
) -> TokenStream {
    //TODO assertion: it is *not* possible to do anything other than to treat
    //  values inside of a Vec<Expression> as ArgPassingStyle::Move
    //  1. is this true
    //      if so, is it enforces at compile time (just throw an error here
    //      or do it on the first pass.
    //  2. if not, good.
    //  regardless inner must just return the parsed Expression. which is why
    //  the value of inner that is passed in is quote! { #arg_name }
    //  if the rust_native type is Vec<&mut HashMap> the borrow checker
    //  could get in the way BUT, lifetimes might help...
    //  ***NEED to test
    let an_element_arg_value_type_parsing_code =
        parse_argval_value_type(arg_name, passing_style, quote! { #arg_name }, false);
    // TODO
    //  if the above is true, we should only accept Vec<T: TryIntoExpression>
    quote! {{
        let #arg_name = #arg_name
            .iter()
            .map(|#arg_name| {
                #an_element_arg_value_type_parsing_code
            })
            .collect::<crate::LispResult<#ty>>()?;
        #inner
    }}
}

/// for Option<Expression> values the ref_exp must first be parsed as an
/// Option, and only in the case that the option is Some will it be
/// necessary to match against every ExpEnum variant.
fn parse_argval_optional_type(
    arg_name: &Ident,
    passing_style: ArgPassingStyle,
    inner: TokenStream,
) -> TokenStream {
    let some_inner = quote! {
        let #arg_name = Some(#arg_name);
        #inner
    };
    // in the case that the value is some, which means the Expression is no longer
    // wrapped in Option, the parse_argval_value_type can be repurposed but
    // with the caveat that after the value of inner it is handed first wraps
    // the matched ExpEnum in Some bound to the #arg_name like the
    // rust native function expects.
    let some_arg_value_type_parsing_code =
        parse_argval_value_type(arg_name, passing_style, some_inner, false);
    quote! {
    match #arg_name {
        None => {
            let #arg_name = None;
            #inner
        }
        Some(#arg_name) => {
           #some_arg_value_type_parsing_code
        }
    }}
}

/// for regular Expression values (no Optional/VarArgs) ref_exp
/// just needs to be matched based on it's ExpEnum variant.
fn parse_argval_value_type(
    arg_name: &Ident,
    passing_style: ArgPassingStyle,
    inner: TokenStream,
    match_should_return: bool,
) -> TokenStream {
    let reference_tokens = tokens_for_matching_references(arg_name, passing_style);
    let ref_exp = reference_tokens.0;
    let ref_match = reference_tokens.1;
    let final_token = if match_should_return {
        quote! {}
    } else {
        quote! {;}
    };
    quote! {
    match #ref_exp {
        //sl_sh::ExpEnum::True => {
        //    let #arg_name = true;
        //    #inner
        //}
        //sl_sh::ExpEnum::False => {
        //    let #arg_name = false;
        //    #inner
        //}
        //sl_sh::ExpEnum::Float(#ref_match) => {
        //    #inner
        //}
        sl_sh::ExpEnum::Int(#ref_match) => {
            #inner
        }
        _ => {
            return Err(sl_sh::types::LispError::new(format!(" not given enough arguments, expected , got .")));
        }
        //sl_sh::ExpEnum::Symbol(#ref_match, _) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::String(#ref_match, _) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Char(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::CodePoint(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::HashMap(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Process(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::File(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Wrapper(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Regex(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Vector(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Values(#ref_match) => {
        //    #inner
        //}
        //sl_sh::ExpEnum::Nil => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Nil not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::Lambda(#ref_match) => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Lambda not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::Macro(#ref_match) => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Macro not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::Function(#ref_match) => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Function not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::LazyFn(_, _) => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::LazyFn not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::Pair(_, _) => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Pair not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::DeclareDef => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::DeclareDef not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::DeclareVar => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::DeclareVar not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::DeclareFn => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::DeclareFn not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::DeclareMacro => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::DeclareMacro not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::Quote => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Quote not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::BackQuote => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::BackQuote not supported as input to sl_sh_fn proc macro."))
        //}
        //sl_sh::ExpEnum::Undefined => {
        //    sl_sh::types::LispError::new(format!("ExpEnum::Undefined not supported as input to sl_sh_fn proc macro."))
        //}
    }#final_token}
}

/// create the nested match statements to parse rust types into sl_sh types.
/// the rust types will determine what sl_sh functions will be used for
/// transformation. If this function throws errors it means that the
/// inputs, val/passing style are wrong and aren't matching to the ArgType(s)
/// properly, or the rust type lookup function is busted.
fn parse_type(
    ty: &syn::TypePath,
    inner: TokenStream,
    val: ArgVal,
    arg_name: &syn::Ident,
    passing_style: ArgPassingStyle,
    outer_parse: fn(&Ident, TokenStream) -> TokenStream,
) -> MacroResult<TokenStream> {
    let tokens = match val {
        ArgVal::Value => parse_argval_value_type(arg_name, passing_style, inner, true),
        ArgVal::Optional => parse_argval_optional_type(arg_name, passing_style, inner),
        ArgVal::Vec => parse_argval_varargs_type(ty, arg_name, passing_style, inner),
    };
    Ok(outer_parse(arg_name, tokens))
}

fn parse_src_function_arguments(original_item_fn: &syn::ItemFn) -> MacroResult<Vec<Arg>> {
    let mut parsed_args = vec![];
    let len = original_item_fn.sig.inputs.len();
    let mut arg_names = vec![];
    for i in 0..len {
        let parse_name = "arg_".to_string() + &i.to_string();
        let parse_name = Ident::new(&parse_name, Span::call_site());
        arg_names.push(parse_name);
    }

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
                            let passing_style = if ty_ref.mutability.is_some() {
                                ArgPassingStyle::MutReference
                            } else {
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

fn generate_sl_sh_fn2(
    original_item_fn: &syn::ItemFn,
    attr_args: syn::AttributeArgs,
) -> MacroResult<TokenStream> {
    let (fn_name, fn_name_attr, eval_values) = parse_attributes(original_item_fn, attr_args)?;
    let doc_comments = get_documentation_for_fn(original_item_fn)?;
    let args_len = original_item_fn.sig.inputs.len();
    let (original_fn_name, builtin_name, parse_name, intern_name) = get_fn_names(original_item_fn);

    let make_args = if eval_values {
        quote! {
            let args = sl_sh::builtins_util::make_args(environment, args)?;
        }
    } else {
        quote! {
            let args = sl_sh::builtins_util::make_args_eval_no_values(environment, args)?;
        }
    };

    let args = parse_src_function_arguments(original_item_fn)?;
    let builtin_fn = generate_builtin_fn2(original_item_fn, &original_fn_name, &builtin_name)?;
    are_args_valid(original_item_fn, &args.as_slice())?;
    let arg_types = to_arg_types(&args.as_slice());

    let (return_type, lisp_result) = get_return_type2(original_item_fn)?;

    // these tokens are for calling the wrapper of the rust native function.
    // Rust native functions may either return a lisp result, a value, or nothing.
    let returns_none = "()" == return_type.to_token_stream().to_string();
    let builtin_call_tokens = match (return_type, lisp_result, returns_none) {
        (_, _, true) | (None, None, false) => {
            quote! {
                #builtin_name.call_expand_args(params)?;
                Ok(sl_sh::types::Expression::make_nil())
            }
        }
        (Some(_), Some(_), false) => {
            quote! {
                #builtin_name.call_expand_args(params).map(Into::into)
            }
        }
        (Some(_), None, false) => {
            quote! {
                Ok(#builtin_name.call_expand_args(params).into())
            }
        }
        (None, Some(_), false) => {
            unreachable!("If this functions returns a LispResult it must also return a value.");
        }
    };

    // directly in this TokenStream enumerate the functions parse_ and intern_
    // parse is responsible for using the environment to turn the Expressions
    // into a list of ArgTypes. ArgTypes are the abstraction that allow
    // Optional and VarArgs type parameters in native rust functions.
    let tokens = quote! {
        #builtin_fn

        fn #parse_name(
            environment: &mut sl_sh::environment::Environment,
            args: &mut dyn Iterator<Item = sl_sh::types::Expression>,
        ) -> sl_sh::LispResult<sl_sh::types::Expression> {
            use std::convert::TryInto;
            use sl_sh::builtins_util::ExpandVecToArgs;
            #make_args
            let #fn_name_attr = #fn_name;
            const args_len: usize = #args_len;
            #arg_types
            let args = sl_sh::get_arg_types(fn_name, arg_types, args)?;
            if args.len() == args_len {
                match args.try_into() {
                    Ok(params) => {
                        let params: [sl_sh::ArgType; args_len] = params;
                        #builtin_call_tokens
                    },
                    Err(e) => {
                        Err(sl_sh::types::LispError::new(format!("{} is broken and can't parse its arguments..", #fn_name_attr, )))
                    }
                }
            } else if args.len() > args_len {
                Err(sl_sh::types::LispError::new(format!("{} given too many arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            } else {
                Err(sl_sh::types::LispError::new(format!("{} not given enough arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            }
        }

        fn #intern_name<S: std::hash::BuildHasher>(
            interner: &mut sl_sh::Interner,
            data: &mut std::collections::HashMap<&'static str, (sl_sh::types::Expression, String), S>,
        ) {
            let #fn_name_attr = #fn_name;
            data.insert(
                interner.intern(#fn_name_attr),
                sl_sh::types::Expression::make_function(#parse_name, #doc_comments),
            );
        }
    };
    Ok(tokens)
}

fn to_arg_types(args: &[Arg]) -> TokenStream {
    let mut tokens = vec![];
    for arg in args {
        tokens.push(match (arg.val, arg.passing_style) {
            (ArgVal::Value, ArgPassingStyle::MutReference) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Value,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::MutReference
                }}
            }
            (ArgVal::Optional, ArgPassingStyle::MutReference) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Optional,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::MutReference
                }}
            }
            (ArgVal::Vec, ArgPassingStyle::MutReference) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Vec,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::MutReference
                }}
            }
            (ArgVal::Value, ArgPassingStyle::Reference) => {
                quote! {sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Value,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::Reference
                }}
            }
            (ArgVal::Optional, ArgPassingStyle::Reference) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Optional,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::Reference
                }}
            }
            (ArgVal::Vec, ArgPassingStyle::Reference) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Vec,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::Reference
                }}
            }
            (ArgVal::Value, ArgPassingStyle::Move) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Value,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::Move
                }}
            }
            (ArgVal::Optional, ArgPassingStyle::Move) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Optional,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::Move
                }}
            }
            (ArgVal::Vec, ArgPassingStyle::Move) => {
                quote! { sl_sh::builtins_util::Arg {
                    val: sl_sh::builtins_util::ArgVal::Vec,
                    passing_style: sl_sh::builtins_util::ArgPassingStyle::Move
                }}
            }
        });
    }
    //for arg_type in &arg_types {
    //    println!("{:?}", arg_type);
    //}
    quote! {
        let arg_types = vec![ #(#tokens),* ];
    }
}

#[proc_macro_attribute]
pub fn sl_sh_fn2(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr_args = parse_macro_input!(attr as syn::AttributeArgs);

    let tokens = match syn::parse::<syn::Item>(input) {
        Ok(item) => match &item {
            syn::Item::Fn(original_item_fn) => {
                let generated_code = match generate_sl_sh_fn2(original_item_fn, attr_args) {
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
//  - functions that return... nothing.
// - sl_sh_fn2 macro should check if the rust function is just receiving an
//      Expression type that doesn't need any of the type parsing code,
//      the arg is fine after the outer parse function
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
//  or all functions are required to return LispResult<()> and those should really return Nil,
//  in lisp world.
// - should all macros just use the try_inner_exp_enum pattern?
// - what about a ToString coercion for ExpEnum::Char/String/Symbol for rust functions that require them
//  to turn into Strings.
