use either::Either;
//TODO why am I using these __private versions, is it
// just to skip out on some annoying .into()'s on TokenStream2
use quote::quote;
use quote::ToTokens;
use quote::__private::TokenStream;
use syn::__private::{Span, TokenStream2};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse, parse_macro_input, AngleBracketedGenericArguments, AttributeArgs, Error, FnArg,
    GenericArgument, Ident, Item, ItemFn, Lit, Meta, NestedMeta, Path, PathArguments, PathSegment,
    ReturnType, Type, TypePath, TypeTuple,
};
extern crate static_assertions;

type MacroResult<T> = Result<T, Error>;

const POSSIBLE_RESULT_TYPES: [&str; 1] = ["LispResult"];
const POSSIBLE_ARG_TYPES: [&str; 2] = ["Option", "Vec"];

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

/// returns the option of inner type and the wrapped generic type (None if it's
/// not generic. If there is no return type None, None is returned. Throws
/// an error if the generic return type is not in the list of predefined
/// constants POSSIBLE_RESULT_TYPES.
fn get_return_type(original_item_fn: &ItemFn) -> MacroResult<(Option<Type>, Option<&'static str>)> {
    let return_type = match &original_item_fn.sig.output {
        ReturnType::Default => return Ok((None, None)),
        ReturnType::Type(_ra_arrow, ty) => *ty.clone(),
    };

    if let Some((inner_type, type_path)) = get_generic_argument_from_type(&return_type) {
        //TODO this function does not need to return MacroResult, should enforce LispResult return type at compile time.
        let wrapper = is_valid_generic_type(type_path, POSSIBLE_RESULT_TYPES.as_slice())?;
        match inner_type {
            GenericArgument::Type(ty) => Ok((Some(ty.clone()), Some(wrapper))),
            _ => {
                return Err(Error::new(
                    original_item_fn.span(),
                    format!(
                        "Functions must return generic arguments of type {:?}.",
                        &POSSIBLE_RESULT_TYPES
                    ),
                ));
            }
        }
    } else {
        Ok((Some(return_type), None))
    }
}

fn opt_is_valid_generic_type<'a>(
    type_path: &TypePath,
    possible_types: &'a [&str],
) -> Option<&'a str> {
    if type_path.path.segments.len() == 1 && type_path.path.segments.first().is_some() {
        let path_segment = &type_path.path.segments.first().unwrap();
        let ident = &path_segment.ident;
        for type_name in possible_types {
            if ident == type_name {
                return Some(type_name);
            }
        }
    }
    None
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
    Err(Error::new(
            type_path.span(),
            format!(
                "Functions of with generic arguments of type {:?} must contain Types, see GenericArgument.",
                possible_types
            ),
        ))
}

fn get_generic_argument_from_type_path(
    type_path: &TypePath,
) -> Option<(&GenericArgument, &TypePath)> {
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

fn get_generic_argument_from_type(ty: &Type) -> Option<(&GenericArgument, &TypePath)> {
    if let Type::Path(ref type_path) = ty {
        get_generic_argument_from_type_path(type_path)
    } else {
        None
    }
}

fn generate_assertions_code_for_return_type_conversions(return_type: &Type) -> TokenStream2 {
    let to_return_type = wrap_with_std_convert(build_sl_sh_expression_type(), "Into");
    quote! {
      static_assertions::assert_impl_all!(#return_type: #to_return_type);
    }
}

fn get_attribute_value_with_key(
    original_item_fn: &ItemFn,
    key: &str,
    values: &[(String, String)],
) -> MacroResult<Option<String>> {
    if values.is_empty() {
        Err(Error::new(
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
                    (_, _) => Err(Error::new(
                        meta.span(),
                        "sl_sh_fn requires one name-value pair, 'fn_name'. Supports optional name-value pair 'eval_values = true')",
                    )),
                }
            }
            other => Err(Error::new(
                other.span(),
                "sl_sh_fn only supports one name-value pair attribute argument, 'fn_name'.",
            )),
        },
        other => Err(Error::new(
            other.span(),
            "sl_sh_fn only supports one name-value pair attribute argument, 'fn_name'.",
        )),
    }
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

fn get_arg_val(type_path: &TypePath) -> MacroResult<ArgVal> {
    if let Some((_generic, type_path)) = get_generic_argument_from_type_path(type_path) {
        let wrapper = opt_is_valid_generic_type(type_path, POSSIBLE_ARG_TYPES.as_slice());
        match wrapper {
            Some(wrapper) if wrapper == "Option" => {
                return Ok(ArgVal::Optional);
            }
            Some(wrapper) if wrapper == "Vec" => {
                return Ok(ArgVal::Vec);
            }
            _ => {}
        }
    }
    Ok(ArgVal::Value)
}

fn parse_value(arg_name: &Ident, inner: TokenStream) -> TokenStream {
    quote! {
        crate::ret_err_exp_enum!(
                #arg_name,
                crate::ArgType::Exp(#arg_name),
                #inner,
                "sl_sh_fn macro is broken. ArgType::Exp can't be parsed as ArgType::Exp"
            )
    }
}

fn parse_optional(arg_name: &Ident, inner: TokenStream) -> TokenStream {
    quote! {
        crate::ret_err_exp_enum!(
                #arg_name,
                crate::ArgType::Opt(#arg_name),
                #inner,
                "sl_sh_fn macro is broken. ArgType::Opt can't be parsed as ArgType::Opt"
            )
    }
}

fn parse_varargs(arg_name: &Ident, inner: TokenStream) -> TokenStream {
    quote! {
        crate::ret_err_exp_enum!(
                #arg_name,
                crate::ArgType::VarArgs(#arg_name),
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
    takes_env: bool,
    original_item_fn: &ItemFn,
    original_fn_name: &Ident,
    arg_names: Vec<Ident>,
) -> MacroResult<TokenStream> {
    // the original function call must return an Expression object
    // this means all returned rust native types must implement TryIntoExpression
    // this is nested inside the builtin expression which must always
    // return a LispResult.
    let takes_env = if takes_env {
        quote! {env, } // env is the name that is passed in to this function
    } else {
        quote! {}
    };
    let (return_type, lisp_result) = get_return_type(original_item_fn)?;
    let returns_none = "()" == return_type.to_token_stream().to_string();
    let original_fn_call = match (return_type, lisp_result, returns_none) {
        // coerce to a LispResult<Expression>
        (Some(_), Some(_), true) => quote! {
            #original_fn_name(#takes_env #(#arg_names),*)?;
            Ok(crate::types::Expression::make_nil())
        },
        (Some(_), Some(_), false) => quote! {
            #original_fn_name(#takes_env #(#arg_names),*).map(Into::into)
        },
        // coerce to Expression
        (Some(_), None, _) => quote! {
            Ok(#original_fn_name(#takes_env #(#arg_names),*).into())
        },
        (None, Some(_), _) => {
            unreachable!("If this functions returns a LispResult it must also return a value.");
        }
        // no return
        (None, None, _) => quote! {
            #original_fn_name(#takes_env #(#arg_names),*);
            Ok(crate::types::Expression::make_nil())
        },
    };
    Ok(quote! {
        #original_fn_call
    })
}

fn make_arg_types(
    original_item_fn: &ItemFn,
    takes_env: bool,
) -> MacroResult<(Vec<Ident>, Vec<TokenStream>)> {
    let len = if takes_env {
        original_item_fn.sig.inputs.len() - 1
    } else {
        original_item_fn.sig.inputs.len()
    };
    let mut arg_names = vec![];
    let mut arg_types = vec![];
    for i in 0..len {
        let parse_name = "arg_".to_string() + &i.to_string();
        let parse_name = Ident::new(&parse_name, Span::call_site());
        arg_names.push(parse_name);
        arg_types.push(quote! { crate::ArgType })
    }
    Ok((arg_names, arg_types))
}

/// return a code for how to refer to the inner exp enum referent type in
/// an function call.
fn tokens_for_matching_references(passing_style: ArgPassingStyle, ty: &TypePath) -> TokenStream {
    match passing_style {
        ArgPassingStyle::Move => quote! {#ty},
        ArgPassingStyle::Reference => quote! {&#ty},
        ArgPassingStyle::MutReference => quote! {& mut #ty},
    }
}

fn parse_argval_varargs_type(
    ty: &TypePath,
    fn_name_attr: &Ident,
    arg_name: &Ident,
    inner: TokenStream,
) -> TokenStream {
    let wrapped_ty = get_type_or_wrapped_type(ty);
    match wrapped_ty {
        Either::Left(wrapped_ty) => {
            quote! {{
                use crate::builtins_util::TryIntoExpression;
                static_assertions::assert_impl_all!(crate::types::Expression: crate::builtins_util::TryIntoExpression<#wrapped_ty>);
                let #arg_name = #arg_name
                    .iter()
                    .map(|#arg_name| {
                        #arg_name.clone().try_into_for(#fn_name_attr)
                    })
                    .collect::<crate::LispResult<#ty>>()?;
                #inner
            }}
        }
        Either::Right(type_tuple) => {
            quote! {}
        }
    }
}

/// for Option<Expression> values the ref_exp must first be parsed as an
/// Option, and only in the case that the option is Some will it be
/// necessary to match against every ExpEnum variant.
fn parse_argval_optional_type(
    ty: &TypePath,
    fn_name_attr: &Ident,
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
        parse_argval_value_type(ty, fn_name_attr, arg_name, passing_style, some_inner);
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

/// at this point the macro is only operating on types it expects
/// which are any rust types, any rust types wrapped in Option,
/// and any rust types wrapped in Vec. If in the future this is
/// confusing wrapper types can be made, i.e. SlshVarArgs,
/// so normal rust Vec could be used without being turned into
/// a SlshVarArgs
/// TODO
///     we need to allow for TypeTuple here.
fn get_type_or_wrapped_type(ty: &TypePath) -> Either<&TypePath, &TypeTuple> {
    let orig_ty = ty;
    if let Some((ty, type_path)) = get_generic_argument_from_type_path(ty) {
        let wrapper = opt_is_valid_generic_type(type_path, POSSIBLE_ARG_TYPES.as_slice());
        match (ty, wrapper) {
            (GenericArgument::Type(ty), Some(_)) => match ty {
                Type::Path(path) => Either::Left(path),
                Type::Tuple(type_tuple) => Either::Right(type_tuple),
                _ => Either::Left(orig_ty),
            },
            (_, _) => Either::Left(orig_ty),
        }
    } else {
        Either::Left(ty)
    }
}

/// for regular Expression values (no Optional/VarArgs) ref_exp
/// just needs to be matched based on it's ExpEnum variant.
fn parse_argval_value_type(
    ty: &TypePath,
    fn_name_attr: &Ident,
    arg_name: &Ident,
    passing_style: ArgPassingStyle,
    inner: TokenStream,
) -> TokenStream {
    let ty = get_type_or_wrapped_type(ty);
    match ty {
        Either::Left(ty) => {
            let str = ty.to_token_stream().to_string();
            // handle &str differently, want impl RustProcedure<F> for TypedWrapper<&str>
            // w/o this special case it generate RustProcedureRef on an unsized TypedWrapper<str>
            let (fn_ref, passing_style, ty) = if str == "str" && passing_style == ArgPassingStyle::Reference
            {
                let passing_style = ArgPassingStyle::Move;
                (quote! { &#ty }, passing_style, quote! { &#ty })
            } else {
                (
                    tokens_for_matching_references(passing_style, ty),
                    passing_style,
                    quote! { #ty },
                )
            };
            let inner = quote! {
                let mut typed_data: crate::types::TypedWrapper<#ty, crate::types::Expression> =
                    crate::types::TypedWrapper::new(#arg_name);
                let callback = |#arg_name: #fn_ref| -> crate::LispResult<crate::types::Expression> {
                    #inner
                };
            };

            match passing_style {
                ArgPassingStyle::Move => {
                    quote! {{
                use crate::types::RustProcedure;
                #inner
                typed_data.apply(#fn_name_attr, callback)
            }}
                }
                _ => {
                    // some reference
                    quote! {{
                use crate::types::RustProcedureRef;
                #inner
                typed_data.apply_ref_mut(#fn_name_attr, callback)
            }}
                }
            }
        }
        Either::Right(type_tuple) => {
            quote! {}
        }
    }
}

/// create the nested match statements to parse rust types into sl_sh types.
/// the rust types will determine what sl_sh functions will be used for
/// transformation. If this function throws errors it means that the
/// inputs, val/passing style are wrong and aren't matching to the ArgType(s)
/// properly, or the rust type lookup function is busted.
fn parse_type(
    ty: &TypePath,
    fn_name_attr: &Ident,
    inner: TokenStream,
    val: ArgVal,
    arg_name: &Ident,
    passing_style: ArgPassingStyle,
    outer_parse: fn(&Ident, TokenStream) -> TokenStream,
) -> MacroResult<TokenStream> {
    let tokens = match val {
        ArgVal::Value => parse_argval_value_type(ty, fn_name_attr, arg_name, passing_style, inner),
        ArgVal::Optional => {
            parse_argval_optional_type(ty, fn_name_attr, arg_name, passing_style, inner)
        }
        ArgVal::Vec => parse_argval_varargs_type(ty, fn_name_attr, arg_name, inner),
    };
    Ok(outer_parse(arg_name, tokens))
}

fn to_arg_types(args: &[Arg]) -> TokenStream {
    let mut tokens = vec![];
    for arg in args {
        tokens.push(match (arg.val, arg.passing_style) {
            (ArgVal::Value, ArgPassingStyle::MutReference) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Value,
                    passing_style: crate::builtins_util::ArgPassingStyle::MutReference
                }}
            }
            (ArgVal::Optional, ArgPassingStyle::MutReference) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Optional,
                    passing_style: crate::builtins_util::ArgPassingStyle::MutReference
                }}
            }
            (ArgVal::Vec, ArgPassingStyle::MutReference) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Vec,
                    passing_style: crate::builtins_util::ArgPassingStyle::MutReference
                }}
            }
            (ArgVal::Value, ArgPassingStyle::Reference) => {
                quote! {crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Value,
                    passing_style: crate::builtins_util::ArgPassingStyle::Reference
                }}
            }
            (ArgVal::Optional, ArgPassingStyle::Reference) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Optional,
                    passing_style: crate::builtins_util::ArgPassingStyle::Reference
                }}
            }
            (ArgVal::Vec, ArgPassingStyle::Reference) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Vec,
                    passing_style: crate::builtins_util::ArgPassingStyle::Reference
                }}
            }
            (ArgVal::Value, ArgPassingStyle::Move) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Value,
                    passing_style: crate::builtins_util::ArgPassingStyle::Move
                }}
            }
            (ArgVal::Optional, ArgPassingStyle::Move) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Optional,
                    passing_style: crate::builtins_util::ArgPassingStyle::Move
                }}
            }
            (ArgVal::Vec, ArgPassingStyle::Move) => {
                quote! { crate::builtins_util::Arg {
                    val: crate::builtins_util::ArgVal::Vec,
                    passing_style: crate::builtins_util::ArgPassingStyle::Move
                }}
            }
        });
    }
    quote! {
        let arg_types = vec![ #(#tokens),* ];
    }
}

/// write the intern_ function code. This code is generated to be called within sl-sh to avoid writing
/// boilerplate code to submit a function symbol and the associated code to the runtime. Every builtin
/// function must be inserted into a hashmap where the key is the name of the function and the value
/// is a function expression that stores the name of the rust function to call and its documentation.
/// It looks like the following in all cases:
/// ```
/// fn intern_one_int_to_float<S: std::hash::BuildHasher>(
///    interner: &mut sl_sh::Interner,
///    data: &mut std::collections::HashMap<&'static str, (sl_sh::types::Expression, String), S>,
///) {
///    let fn_name = "oneintofloat";
///    data.insert(
///        interner.intern(fn_name),
///        sl_sh::types::Expression::make_function(parse_one_int_to_float, " my docs\n"),
///    );
///}
/// ```
fn generate_intern_fn(
    original_fn_name_str: &str,
    fn_name_attr: &Ident,
    fn_name: &str,
    doc_comments: String,
) -> TokenStream {
    let parse_name = get_parse_fn_name(original_fn_name_str);
    let intern_name = get_intern_fn_name(original_fn_name_str);
    quote! {
        fn #intern_name<S: std::hash::BuildHasher>(
            interner: &mut crate::Interner,
            data: &mut std::collections::HashMap<&'static str, (crate::types::Expression, String), S>,
        ) {
            let #fn_name_attr = #fn_name;
            data.insert(
                interner.intern(#fn_name_attr),
                crate::types::Expression::make_function(#parse_name, #doc_comments),
            );
        }
    }
}

/// write the parse_ version of the provided function. The function it generates takes an environment
/// and a list of Expressions, evaluates those expressions and then maps the provided list of expressions
/// to a list of ArgType values. To accomplish this information from compile time, arg_types,
/// is manually inserted into this function. This way the evaluated list of args and the expected
/// list of args can be compared and the appropriate vector of arguments can be created and
/// passed to the builtin function. To map a vector of ArgType structs to an actual function
/// call the ExpandVecToArgs trait is used. A sample parse_ function for a function that takes
/// one argument is shown below.
/// ```
/// fn parse_one_int_to_float(
///     environment: &mut sl_sh::environment::Environment,
///     args: &mut dyn Iterator<Item = sl_sh::types::Expression>,
/// ) -> sl_sh::LispResult<sl_sh::types::Expression> {
///     use sl_sh::builtins_util::ExpandVecToArgs;
///     use std::convert::TryInto;
///     let args = sl_sh::builtins_util::make_args_eval_no_values(environment, args)?;
///     let fn_name = "one-in-to-float";
///     const args_len: usize = 1usize;
///     // this arg_types variable is generated by the macro for use at runtime.
///     let arg_types = vec![sl_sh::builtins_util::Arg {
///         val: sl_sh::builtins_util::ArgVal::Value,
///         passing_style: sl_sh::builtins_util::ArgPassingStyle::Move,
///     }];
///
///     let args = crate::builtins_util::make_args_eval_no_values(environment, args)?;
///     let args = sl_sh::get_arg_types(fn_name, arg_types, args)?;
///     if args.len() == args_len {
///         match args.try_into() {
///             Ok(params) => {
///                 // use const generics and blanket implementation of ExpandvecToArgs over
///                 // function calls to map vector to function call.
///                 let params: [sl_sh::ArgType; args_len] = params;
///                 builtin_one_int_to_float.call_expand_args(params)
///             }
///             Err(e) => Err(sl_sh::types::LispError::new(format!(
///                 "{} is broken and can't parse its arguments.",
///                 fn_name
///             ))),
///         }
///     } else if args.len() > args_len {
///         Err(sl_sh::types::LispError::new(format!(
///             "{}  given too many arguments, expected {}, got, {}.",
///             fn_name,
///             args_len,
///             args.len()
///         )))
///     } else {
///         Err(sl_sh::types::LispError::new(format!(
///             "{}  not given enough arguments, expected {}, got, {}.",
///             fn_name,
///             args_len,
///             args.len()
///         )))
///     }
/// }
/// ```
fn generate_parse_fn(
    original_fn_name_str: &str,
    eval_values: bool,
    fn_name_attr: &Ident,
    fn_name: &str,
    args_len: usize,
    args: &[Arg],
) -> TokenStream {
    let parse_name = get_parse_fn_name(original_fn_name_str);
    let builtin_name = get_builtin_fn_name(original_fn_name_str);
    let arg_types = to_arg_types(args);

    let make_args = if eval_values {
        quote! {
            let args = crate::builtins_util::make_args(environment, args)?;
        }
    } else {
        quote! {
            let args = crate::builtins_util::make_args_eval_no_values(environment, args)?;
        }
    };

    quote! {
        fn #parse_name(
            environment: &mut crate::environment::Environment,
            args: &mut dyn Iterator<Item = crate::types::Expression>,
        ) -> crate::LispResult<crate::types::Expression> {
            use std::convert::TryInto;
            use crate::builtins_util::ExpandVecToArgs;
            #make_args
            let #fn_name_attr = #fn_name;
            const args_len: usize = #args_len;
            #arg_types
            let args = crate::get_arg_types(fn_name, arg_types, args)?;
            if args.len() == args_len {
                match args.try_into() {
                    Ok(params) => {
                        let params: [crate::ArgType; args_len] = params;
                        #builtin_name.call_expand_args(environment, params)
                    },
                    Err(e) => {
                        Err(crate::types::LispError::new(format!("{} is broken and can't parse its arguments.", #fn_name_attr, )))
                    }
                }
            } else if args.len() > args_len {
                Err(crate::types::LispError::new(format!("{} given too many arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            } else {
                Err(crate::types::LispError::new(format!("{} not given enough arguments, expected {}, got {}.", #fn_name_attr, args_len, args.len())))
            }
        }
    }
}

/// write the builtin_ version of the provided function. This function is the function taht makes
/// a direct call to the original rust native function to which the macro was applied. To accomplish
/// this the builtin_ function generates takes some number of ArgType structs (the wrapper enum that
/// enables passing optional and varargs). the body of the function handles unwrapping the ArgType
/// variables and then unwrapping the Expressions those contain into the proper rust native
/// function. The process is done in a for loop but it recursively builds the body of builtin_
/// by passing around a token stream.
///
/// The token stream is initialized with code to call to the original rust native function with
/// pre-generated names for each of the arguments, e.g. `my_rust_native_function(arg_0, arg_1);`.
/// Each subsequent iteration of the loop takes the previous token stream returned by the loop and
/// uses that as it's innermost scope. Thus the original function call is at the core of a series
/// of scopes that create all the necessary arguments with the proper types that were specified on
/// initialization. For a function of one argument that means the code would look something like:
/// ```
/// use sl_sh_proc_macros::sl_sh_fn;
/// fn builtin_one_int_to_float(arg_0: crate::ArgType) -> crate::LispResult<crate::types::Expression> {
///    const _: fn() = || {
///        fn assert_impl_all<T: ?Sized + std::convert::Into<crate::Expression>>() {}
///        assert_impl_all::<f64>();
///    };
///    let fn_name = "one-int-to-float";
///    match arg_0 {
///        crate::ArgType::Exp(arg_0) => {
///            use crate::types::RustProcedure;
///            let mut typed_data: crate::types::TypedWrapper<i64, crate::types::Expression> =
///                crate::types::TypedWrapper::new(arg_0);
///            let callback = |arg_0: i64| -> crate::LispResult<crate::types::Expression> {
///                one_int_to_float(arg_0).map(Into::into)
///            };
///            typed_data.apply(fn_name, callback)
///        }
///        _ => {
///            return Err(LispError::new(
///                "sl_sh_fn macro is broken. ArgType::Exp can't be parsed as ArgType::Exp",
///            ));
///        }
///    }
///}
/// #[sl_sh_fn(fn_name = "one-int-to-float")]
/// fn one_int_to_float(int: i64) -> LispResult<f64> {
///    Ok(int as f64)
/// }
/// ```
fn generate_builtin_fn(
    original_item_fn: &ItemFn,
    original_fn_name_str: &str,
    fn_name: &str,
    fn_name_attr: &Ident,
    takes_env: bool,
) -> MacroResult<TokenStream> {
    let original_fn_name = Ident::new(original_fn_name_str, Span::call_site());
    let builtin_name = get_builtin_fn_name(original_fn_name_str);
    let (arg_names, arg_types) = make_arg_types(original_item_fn, takes_env)?;

    let orig_fn_call = make_orig_fn_call(
        takes_env,
        original_item_fn,
        &original_fn_name,
        arg_names.clone(),
    )?;
    // initialize the innermost token stream to the code of the original_fn_call
    let mut prev_token_stream = orig_fn_call;
    let skip = if takes_env { 1 } else { 0 };
    let fn_args = original_item_fn
        .sig
        .inputs
        .iter()
        .skip(skip)
        .zip(arg_names.iter());
    for (fn_arg, ident) in fn_args {
        if let FnArg::Typed(ty) = fn_arg {
            match &*ty.ty {
                //TODO how to support vec/option of type_tuple?
                Type::Path(ty) => {
                    let val = get_arg_val(ty)?;
                    let parse_layer_1 = get_parser_for_arg_val(val);
                    let passing_style = ArgPassingStyle::Move;
                    prev_token_stream = parse_type(
                        ty,
                        fn_name_attr,
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
                            fn_name_attr,
                            prev_token_stream.clone(),
                            val,
                            ident,
                            passing_style,
                            parse_layer_1,
                        )?;
                    }
                    Type::Tuple(type_tuple) => {
                        let val = ArgVal::Value;
                        let parse_layer_1 = get_parser_for_arg_val(val);
                        let passing_style = if ty_ref.mutability.is_some() {
                            ArgPassingStyle::MutReference
                        } else {
                            ArgPassingStyle::Reference
                        };
                        parse_type_tuple(
                            type_tuple,
                            fn_name_attr,
                            prev_token_stream.clone(),
                            val,
                            ident,
                            passing_style,
                            parse_layer_1,
                        );
                    }
                    _ => {}
                },
                Type::Tuple(type_tuple) => {
                    let val = ArgVal::Value;
                    let parse_layer_1 = get_parser_for_arg_val(val);
                    let passing_style = ArgPassingStyle::Move;
                    parse_type_tuple(
                        type_tuple,
                        fn_name_attr,
                        prev_token_stream.clone(),
                        val,
                        ident,
                        passing_style,
                        parse_layer_1,
                    );
                }
                _ => {
                    panic!("Found unsupported parameter type, allowed Type, a reference of a Type, a Tuple, or a reference to a Tuple.")
                }
            }
        }
    }
    let (return_type, _) = get_return_type(original_item_fn)?;
    let mut conversions_assertions_code = vec![];
    if let Some(return_type) = return_type {
        conversions_assertions_code.push(generate_assertions_code_for_return_type_conversions(
            &return_type,
        ));
    }
    let tokens = quote! {
        fn #builtin_name(env: &mut crate::environment::Environment, #(#arg_names: #arg_types),*) -> crate::LispResult<crate::types::Expression> {
            #(#conversions_assertions_code)*
            let #fn_name_attr = #fn_name;
            #prev_token_stream
        }
    };
    Ok(tokens)
}

fn parse_type_tuple(
    type_tuple: &TypeTuple,
    fn_name_attr: &Ident,
    inner: TokenStream,
    val: ArgVal,
    arg_name: &Ident,
    passing_style: ArgPassingStyle,
    outer_parse: fn(&Ident, TokenStream) -> TokenStream,
) -> TokenStream {
    // TODO once builtins_function inner loop is it's own function,
    // harness it's power to efficiently loop over the types in
    // type_typle.elems.
    //if &type_tuple.elems.len() != 2 {
    //    for ty in &type_tuple.elems {
    //        match ty {
    //            Type::Path(type_path) => {
    //                
    //            }
    //            Type::Reference(ty_ref) => {
    //                
    //            }
    //        }
    //        let tokens = match val {
    //            ArgVal::Value => parse_argval_value_type(ty, fn_name_attr, arg_name, passing_style, inner),
    //            ArgVal::Optional => {
    //                parse_argval_optional_type(ty, fn_name_attr, arg_name, passing_style, inner)
    //            }
    //            ArgVal::Vec => parse_argval_varargs_type(ty, fn_name_attr, arg_name, inner),
    //        };
    //    }
    //} else {
    //    panic!("Only typles with two elements are supported.");
    //}
    //Ok(outer_parse(arg_name, tokens))
    quote!{}
}

/// Optional and Vec types are supported to create the idea of items that might be provided or
/// for providing a list of zero or more items that can be passed in, a varargs type, or both.
/// Because the nature of optional and varargs are context dependent, e.g. variable numbers of
/// arguments would have to be at the end of the function signature, otherwise, it's much harder
/// to specify which arguments are required.
fn are_args_valid(original_item_fn: &ItemFn, args: &[Arg], takes_env: bool) -> MacroResult<()> {
    if args.is_empty() || (!takes_env && args.len() == 1 || takes_env && args.len() == 2) {
        Ok(())
    } else {
        let mut found_opt = false;
        let mut found_value = false;
        for (i, arg) in args.iter().rev().enumerate() {
            match (i, arg.val, found_opt, found_value) {
                (i, ArgVal::Vec, _, _) if i > 0 => {
                    return Err(Error::new(
                        original_item_fn.span(),
                        "Only one Vec argument is supported and it must be the last argument.",
                    ));
                }
                (_, ArgVal::Optional, _, true) => {
                    return Err(Error::new(
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

/// Create a Vec<Arg> from the original fn's signature. Information is needed at compile and
/// run time to translate the list of sl_sh expressions to rust native types. This Arg types
/// stores the information about the rust native type (Value/Option/Var) as well as whether it's moved, passed
/// by reference, or passed by mutable reference.
fn parse_src_function_arguments(
    original_item_fn: &ItemFn,
    takes_env: bool,
) -> MacroResult<Vec<Arg>> {
    let mut parsed_args = vec![];
    let len = if takes_env {
        original_item_fn.sig.inputs.len() - 1
    } else {
        original_item_fn.sig.inputs.len()
    };
    let mut arg_names = vec![];
    for i in 0..len {
        let parse_name = "arg_".to_string() + &i.to_string();
        let parse_name = Ident::new(&parse_name, Span::call_site());
        arg_names.push(parse_name);
    }

    let skip = if takes_env { 1 } else { 0 };

    for fn_arg in original_item_fn.sig.inputs.iter().skip(skip) {
        match fn_arg {
            FnArg::Receiver(_) => {
                return Err(Error::new(
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
                            return Err(Error::new(
                                original_item_fn.span(),
                                &format!(
                                    "Only references/arguments of type path and reference are supported.: {:?})), ",
                                    ty.to_token_stream()
                                ),
                            ))
                        }
                    }
                }
                Type::Tuple(type_tuple) => {
                    
                }
                _ => {
                    return Err(Error::new(
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

/// return the function names the macro will create. Given a base name, <base>
/// return intern_<base> Ident to be used as function name
fn get_intern_fn_name(original_fn_name: &str) -> Ident {
    let builtin_name = "intern_".to_string() + original_fn_name;
    Ident::new(&builtin_name, Span::call_site())
}

/// return the function names the macro will create. Given a base name, <base>
/// return parse_<base> Ident to be used as function name
fn get_parse_fn_name(original_fn_name: &str) -> Ident {
    let builtin_name = "parse_".to_string() + original_fn_name;
    Ident::new(&builtin_name, Span::call_site())
}

/// return the function names the macro will create. Given a base name, <base>
/// return builtin_<base> Ident to be used as function name
fn get_builtin_fn_name(original_fn_name: &str) -> Ident {
    let builtin_name = "builtin_".to_string() + original_fn_name;
    Ident::new(&builtin_name, Span::call_site())
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
        Err(Error::new(
            original_item_fn.span(),
            "Functions with this attribute included must have documentation.",
        ))
    } else {
        Ok(docs)
    }
}

fn parse_attributes(
    original_item_fn: &ItemFn,
    attr_args: AttributeArgs,
) -> MacroResult<(String, Ident, bool, bool)> {
    let vals = attr_args
        .iter()
        .map(get_attribute_name_value)
        .collect::<MacroResult<Vec<(String, String)>>>()?;
    let fn_name_attr = "fn_name".to_string();
    let fn_name = get_attribute_value_with_key(original_item_fn, &fn_name_attr, vals.as_slice())?
        .ok_or_else(|| {
        Error::new(
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
    let takes_env = if let Some(value) =
        get_attribute_value_with_key(original_item_fn, "takes_env", vals.as_slice())?
    {
        value == "true"
    } else {
        false
    };
    Ok((fn_name, fn_name_attr, eval_values, takes_env))
}

/// this function outputs all of the generated code, it is composed into three different functions:
/// intern_<original_fn_name>
/// parse_<original_fn_name>
/// builtin_<original_fn_name>
/// - intern_ is the simplest function, it is generated to be called within sl-sh to avoid writing
/// boilerplate code to submit a function symbol and the associated code to the runtime.
/// - parse_ has the same function signature as all rust native functions, it takes the environment
/// and a list of evalable args. It evals those arguments at runtime so the resultant expressions
/// can be consumed by the builtin code.
/// - builtin_ is the core of the macro, it takes some number of wrapped expression types prepared by
/// parse and translates those to the appropriate rust types and calls the rust native function.
fn generate_sl_sh_fn(
    original_item_fn: &ItemFn,
    attr_args: AttributeArgs,
) -> MacroResult<TokenStream> {
    let (fn_name, fn_name_attr, eval_values, takes_env) =
        parse_attributes(original_item_fn, attr_args)?;
    let original_fn_name_str = original_item_fn.sig.ident.to_string();
    let original_fn_name_str = original_fn_name_str.as_str();

    let args = parse_src_function_arguments(original_item_fn, takes_env)?;
    are_args_valid(original_item_fn, args.as_slice(), takes_env)?;
    let builtin_fn = generate_builtin_fn(
        original_item_fn,
        original_fn_name_str,
        fn_name.as_str(),
        &fn_name_attr,
        takes_env,
    )?;

    let args_len = if takes_env {
        original_item_fn.sig.inputs.len() - 1
    } else {
        original_item_fn.sig.inputs.len()
    };
    let parse_fn = generate_parse_fn(
        original_fn_name_str,
        eval_values,
        &fn_name_attr,
        fn_name.as_str(),
        args_len,
        args.as_slice(),
    );
    let doc_comments = get_documentation_for_fn(original_item_fn)?;
    let intern_fn = generate_intern_fn(
        original_fn_name_str,
        &fn_name_attr,
        fn_name.as_str(),
        doc_comments,
    );
    let tokens = quote! {
        #builtin_fn

        #parse_fn

        #intern_fn
    };
    Ok(tokens)
}

/// macro that creates a bridge between rust native code and sl-sh code, specify the lisp
/// function name to be interned with the "fn_name" attribute. This macro outputs all of the
/// generated bridge code as well as the original function's code so it can be used
/// by the generated bridge code.
#[proc_macro_attribute]
pub fn sl_sh_fn(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let attr_args = parse_macro_input!(attr as AttributeArgs);

    let tokens = match parse::<Item>(input) {
        Ok(item) => match &item {
            Item::Fn(original_item_fn) => {
                let generated_code = match generate_sl_sh_fn(original_item_fn, attr_args) {
                    Ok(generated_code) => generated_code,
                    Err(e) => e.to_compile_error(),
                };
                let original_fn_code = item.into_token_stream();
                quote! {
                    #generated_code

                    #original_fn_code
                }
            }
            _ => Error::new(item.span(), "This attribute only supports functions.")
                .to_compile_error(),
        },
        Err(e) => Error::new(e.span(), "Failed to parse proc_macro_attribute.").to_compile_error(),
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
//  - support functions that need further manipulation with Environment... make a version
//  of TryIntoExpression that takes environment?
//      - builtins_hashmap.rs
//          + builtin_make_hash
//          + builtin_hash_set
// - what about a ToString coercion for ExpEnum::Char/String/Symbol for rust functions that require them
//  to turn into Strings. somehow need a way to represent a type that can be multiple enums, or a pair,
//  that's also needed for some functions in builtins_hashmap
// - would it be possible to embed environment in TypedWrapper so it's available at runtime? hash_set
// - attribute to somehow delay evaluation of parameter, e.g. builtin_hash_get
//  - functions that return optional.
//  - functions that return Values.
//  - use Option-al arguments
//      - builtins_file.rs
//          + builtin_get_temp_file
//          + builtin_with_temp_dir
//      - builtins_hashmap.rs
//          + builtin_hash_get
//  - use variadic functions
//      - builtins.rs
//          + builtin_apply
//          + builtin_unwind_protect
//      - builtins_types.rs
//          + builtin_to_symbol
