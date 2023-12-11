use quote::quote;
use quote::ToTokens;
use quote::__private::TokenStream;
use std::fmt::{Display, Formatter};
use syn::__private::{Span, TokenStream2};
use syn::spanned::Spanned;
use syn::{
    parse, parse_macro_input, AttributeArgs, Error, FnArg, GenericArgument, Ident, Item, ItemFn,
    Lit, Meta, NestedMeta, PathArguments, ReturnType, Type, TypeBareFn, TypePath, TypeReference,
    TypeTuple,
};
extern crate static_assertions;

// [`RustProcedure`] and [`RustProcedureRefMut`] are traits that are used to implement type conversions
// from [`Value`] to Rust types that take a callback function so the various arguments to rust
// native functions can be curried by recursively applied callbacks doing so -in place- to avoid
// needing to copy the data.
//
//  TODO PC ( this is why the fun() is applied at the core of each statement that matches self in a rust procedure )
//     however, in the new world... if every single tuple item in the [`Value`] enum is copy then
//     maybe it doesn't matter anymore and this strategy can be abandoned.
//
// Is TryIntoExpression still needed?
// It looks like nothing can be converted From Rust Type to Value without vm. So, something new will
// need to be figured out here.

// still struggling w/ compiler about how TryFromSlosh<&str> is going to work.
// since the Value enum is not actually the "actual" thing that owns the data we do not necessarily
// need the approach in sl-sh where a closure was used to prevent needing to return the inner data
// from the Expression enum... but it does need to work!
//
// actually... now I'm not sure that's true, we're not going to get away with returning a reference
// inside try_from_slosh... I think, even if a try_inner_string type macro is introduced (pretty
// sure but think on it more!!!) which means the only choice is to pass in a closure to try_from_slosh
// so that that closure can be passed into the try_inner_string macro.
//
// OR
//
// since nothing is actually owned in the value maybe we could reutrn the ahndle or the value (i64)
// and just remember the type information e.g. what function to call on the vm to extract the value to avoid
// needing to do the extraction inside the try_from_slosh macro...?

//TODO PC
// macro crate wish list
// 1. The macro should fail if the structure of the docs is not as expected. e.g. Type/Namespace/.../Usage/Example/
// 2. type aliasing, need a gensym type macro so I do not conflict with names.
// 3. trybuild tests!
// 4. worry about inlining, e.g. are the mechanisms in place to do the type conversions constant time,
//    and inlined? or is there a way to make them so?
// 5. can yet another crate solve the problem of housing typehandle/passingstyle/param/errorstrings/typedwrapper in the same place?

/// Simple wrapper so the macro can infer the type of the Value at runtime to see if the value
/// provided to the lisp environment was the type of value the rust function expected.


/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is a normal Type, or some
/// supported wrapped type, e.g. Optional.
#[derive(Copy, Clone, Debug, PartialEq)]
enum TypeHandle {
    Direct,
    Optional,
    VarArgs,
}

/// Used by sl_sh_fn macro to embed information at runtime about the parameters of
/// the rust native function, specifically whether it is going to pass the value (a move),
/// a reference, or mutable reference.
#[derive(Copy, Clone, Debug, PartialEq)]
enum PassingStyle {
    Value,
    Reference,
    MutReference,
}

/// Struct used by sl_sh_fn macro to embed information in an array at runtime about each of
/// the parameters of the rust native function.
#[derive(Copy, Clone, Debug, PartialEq)]
struct Param {
    handle: TypeHandle,
    passing_style: PassingStyle,
}

type MacroResult<T> = Result<T, Error>;

const POSSIBLE_RETURN_TYPES: [&str; 2] = ["LispResult", "Option"];
const SPECIAL_ARG_TYPES: [&str; 2] = ["Option", "VarArgs"];
const POSSIBLE_ARG_TYPES: [&str; 3] = ["Option", "VarArgs", "Vec"];

#[derive(Copy, Clone)]
enum SupportedGenericReturnTypes {
    LispResult,
    Option,
}

enum RustType {
    BareFn(TypeBareFn, Span),
    Path(TypePath, Span),
    Tuple(TypeTuple, Span),
    Reference(TypeReference, Span),
    Unsupported(Span),
}

impl RustType {
    pub fn span(&self) -> Span {
        match self {
            RustType::BareFn(_, x) => *x,
            RustType::Path(_, x) => *x,
            RustType::Tuple(_, x) => *x,
            RustType::Reference(_, x) => *x,
            RustType::Unsupported(x) => *x,
        }
    }
}

impl From<Type> for RustType {
    fn from(ty: Type) -> Self {
        match ty {
            Type::BareFn(x) => {
                let span = x.span();
                RustType::BareFn(x, span)
            }
            Type::Path(x) => {
                let span = x.span();
                RustType::Path(x, span)
            }
            Type::Reference(x) => {
                let span = x.span();
                RustType::Reference(x, span)
            }
            Type::Tuple(x) => {
                let span = x.span();
                RustType::Tuple(x, span)
            }
            x => {
                let span = x.span();
                RustType::Unsupported(span)
            }
        }
    }
}

impl Display for SupportedGenericReturnTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SupportedGenericReturnTypes::LispResult => {
                write!(f, "LispResult")
            }
            SupportedGenericReturnTypes::Option => {
                write!(f, "Option")
            }
        }
    }
}

/// returns the option of inner type and the wrapped generic type (None if it's
/// not generic. If there is no return type None, None is returned. Throws
/// an error if the generic return type is not in the list of predefined
/// constants POSSIBLE_RESULT_TYPES.
fn get_return_type(
    original_item_fn: &ItemFn,
) -> MacroResult<(Option<Type>, Option<SupportedGenericReturnTypes>)> {
    let return_type = match &original_item_fn.sig.output {
        ReturnType::Default => return Ok((None, None)),
        ReturnType::Type(_ra_arrow, ty) => *ty.clone(),
    };

    if let Some((inner_type, type_path)) = get_generic_argument_from_type(&return_type) {
        let wrapper = is_valid_generic_type(type_path, POSSIBLE_RETURN_TYPES.as_slice())?;
        match inner_type {
            GenericArgument::Type(ty) => Ok((Some(ty.clone()), Some(wrapper))),
            _ => Err(Error::new(
                original_item_fn.span(),
                format!(
                    "sl_sh_fn macros can only return generic arguments of types {:?}.",
                    &POSSIBLE_RETURN_TYPES
                ),
            )),
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

fn is_valid_generic_type(
    type_path: &TypePath,
    possible_types: &[&str],
) -> MacroResult<SupportedGenericReturnTypes> {
    if type_path.path.segments.len() == 1 && type_path.path.segments.first().is_some() {
        let path_segment = &type_path.path.segments.first().unwrap();
        let ident = &path_segment.ident;
        for type_name in possible_types {
            if ident == type_name {
                if type_name == &SupportedGenericReturnTypes::LispResult.to_string().as_str() {
                    return Ok(SupportedGenericReturnTypes::LispResult);
                } else if type_name == &SupportedGenericReturnTypes::Option.to_string().as_str() {
                    return Ok(SupportedGenericReturnTypes::Option);
                }
            }
        }
    }
    Err(Error::new(
        type_path.span(),
        format!(
            "Functions can only return GenericArguments of type {possible_types:?}, try wrapping this value in Option or LispResult."
        ),
    ))
}

fn get_generic_argument_from_type_path(
    type_path: &TypePath,
) -> Option<(&GenericArgument, &TypePath)> {
    if type_path.path.segments.len() == 1 {
        for path_segment in &type_path.path.segments.iter().next_back() {
            if let PathArguments::AngleBracketed(args) = &path_segment.arguments {
                if let Some(ty) = args.args.iter().next_back() {
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
    quote! {
      static_assertions::assert_impl_all!(#return_type: std::convert::Into<crate::types::Expression>);
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

fn get_bool_attribute_value_with_key(
    original_item_fn: &ItemFn,
    key: &str,
    values: &[(String, String)],
) -> MacroResult<bool> {
    if values.is_empty() {
        Err(Error::new(
            original_item_fn.span(),
            "sl_sh_fn requires at least one name-value pair, 'fn_name = \"<name-of-sl-sh-fun>\"'.",
        ))
    } else {
        for name_value in values {
            if name_value.0 == key {
                return Ok(name_value.1 == "true");
            }
        }
        Ok(false)
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

fn get_type_handle(type_path: &TypePath) -> TypeHandle {
    if let Some((_generic, type_path)) = get_generic_argument_from_type_path(type_path) {
        let wrapper = opt_is_valid_generic_type(type_path, SPECIAL_ARG_TYPES.as_slice());
        match wrapper {
            Some("Option") => {
                return TypeHandle::Optional;
            }
            Some("VarArgs") => {
                return TypeHandle::VarArgs;
            }
            _ => {}
        }
    }
    TypeHandle::Direct
}

fn no_parse_param(
    _arg_name: &Ident,
    inner: TokenStream,
    _param: Param,
    _required_args: usize,
    _idx: usize,
) -> TokenStream {
    inner
}

fn parse_param(
    arg_name: &Ident,
    inner: TokenStream,
    param: Param,
    required_args: usize,
    idx: usize,
) -> TokenStream {
    match param.handle {
        TypeHandle::Direct => {
            quote! {
                let param = arg_types[#idx];
                match param.handle {
                    crate::builtins_util::TypeHandle::Direct => match args.get(#idx) {
                        None => {
                            return Err(crate::types::LispError::new(format!(
                                "{} not given enough arguments, expected at least {} arguments, got {}.",
                                fn_name,
                                #required_args,
                                args.len()
                            )));
                        }
                        Some(#arg_name) => {
                            #inner
                        },
                    },
                    _ => {
                        return Err(crate::types::LispError::new(format!(
                            "{} failed to parse its arguments, internal error.",
                            fn_name,
                        )));
                    },
                }
            }
        }
        TypeHandle::Optional => {
            quote! {
                let param = arg_types[#idx];
                let arg = args.get(#idx);
                match param.handle {
                    crate::builtins_util::TypeHandle::Optional => {
                        let #arg_name = arg.map(|x| x.to_owned());
                        #inner
                    },
                    _ => {
                        return Err(crate::types::LispError::new(format!(
                            "{} failed to parse its arguments, internal error.",
                            fn_name,
                        )));
                    },
                }
            }
        }
        TypeHandle::VarArgs => {
            quote! {
                let param = arg_types[#idx];
                let arg = args.get(#idx);
                match param.handle {
                    crate::builtins_util::TypeHandle::VarArgs => {
                        let #arg_name = args[#idx..].iter().map(|x| x.clone()).collect::<Vec<crate::types::Expression>>();
                        #inner
                    },
                    _ => {
                        return Err(crate::types::LispError::new(format!(
                            "{} failed to parse its arguments, internal error.",
                            fn_name,
                        )));
                    }
                }
            }
        }
    }
}

fn get_parser_for_type_handle(
    noop_outer_parse: bool,
) -> fn(&Ident, TokenStream, Param, usize, usize) -> TokenStream {
    match noop_outer_parse {
        true => no_parse_param,
        false => parse_param,
    }
}

/// generate an (optionally inlined) call to the original fn with the names of all of the variables,
/// arg_names, filled in,, e.g. `fn myfn(arg_0, arg_1, arg_2, ...) { ... }`.
/// This function also inserts a dynamic check to throw an error if too many
/// arguments were provided based on the signature of the target function.
fn make_orig_fn_call(
    inline: bool,
    takes_env: bool,
    original_item_fn: &ItemFn,
    original_fn_name: &Ident,
    required_args: usize,
    arg_names: Vec<Ident>,
) -> MacroResult<TokenStream> {
    // the original function call must return an Expression object
    // this means all returned rust native types must implement TryIntoExpression
    // this is nested inside the builtin expression which must always
    // return a LispResult.
    let skip = usize::from(takes_env);
    let takes_env = if takes_env {
        quote! {environment, } // environment is the name that is passed in to this function
    } else {
        quote! {}
    };

    let (return_type, lisp_return) = get_return_type(original_item_fn)?;
    let returns_none = "()" == return_type.to_token_stream().to_string();
    let fn_body = if inline {
        let mut inline_toks = vec![];
        for (fn_arg, arg_name) in original_item_fn
            .sig
            .inputs
            .iter()
            .skip(skip)
            .zip(arg_names.iter())
        {
            match fn_arg {
                FnArg::Receiver(_) => {}
                FnArg::Typed(typed) => {
                    let pat = typed.pat.clone();
                    let ty = typed.ty.clone();
                    let binding = quote! {
                        let #pat: #ty = #arg_name;
                    };
                    inline_toks.push(binding);
                }
            }
        }

        let block = original_item_fn.block.clone();
        match &original_item_fn.sig.output {
            ReturnType::Default => {
                quote! {{
                    #(#inline_toks)*
                    let res = {
                        #block
                    };
                    res
                }}
            }
            ReturnType::Type(_ra_arrow, ty) => {
                quote! {{
                    #(#inline_toks)*
                    let res: #ty = {
                        #block
                    };
                    res
                }}
            }
        }
    } else {
        quote! {
            #original_fn_name(#takes_env #(#arg_names),*)
        }
    };

    let original_fn_call = match (return_type, lisp_return, returns_none) {
        (Some(_), Some(SupportedGenericReturnTypes::LispResult), true) => quote! {
            #fn_body?;
            return Ok(crate::types::Expression::make_nil());
        },
        (Some(_), Some(SupportedGenericReturnTypes::Option), true) => quote! {
            #fn_body;
            return Ok(crate::types::Expression::make_nil());
        },
        (Some(_), Some(SupportedGenericReturnTypes::LispResult), false) => quote! {
            return #fn_body.map(Into::into);
        },
        (Some(_), Some(SupportedGenericReturnTypes::Option), false) => quote! {
            if let Some(val) = #fn_body {
                return Ok(val.into());
            } else {
                return Ok(crate::types::Expression::make_nil());
            }
        },
        // coerce to Expression
        (Some(_), None, _) => quote! {
            return Ok(#fn_body.into());
        },
        (None, Some(_), _) => {
            unreachable!("If this functions returns a LispResult it must also return a value.");
        }
        // no return
        (None, None, _) => quote! {
            #fn_body;
            return Ok(crate::types::Expression::make_nil());
        },
    };
    let const_params_len = get_const_params_len_ident();
    Ok(quote! {
        match args.get(#const_params_len) {
            Some(_) if #const_params_len == 0 || arg_types[#const_params_len - 1].handle != crate::builtins_util::TypeHandle::VarArgs => {
                return Err(crate::types::LispError::new(format!(
                    "{} given too many arguments, expected at least {} arguments, got {}.",
                    fn_name,
                    #required_args,
                    args.len()
                )));
            }
            _ => {
                #original_fn_call
            }
        }
    })
}

/// create two lists that can be joined by macro syntax to create the inner part of a function
/// signature, e.g. (arg_0: a_type, arg_1: b_type, ...) in some existing rust function:
/// fn myfn(arg_0: a_type, arg_1: b_type, ...) { ... }
fn generate_inner_fn_signature_to_orig_fn_call(
    original_item_fn: &ItemFn,
    takes_env: bool,
) -> MacroResult<Vec<Ident>> {
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
    Ok(arg_names)
}

/// return a code for how to refer to the inner exp enum referent type in
/// an function call.
fn tokens_for_matching_references(passing_style: PassingStyle, ty: &TypePath) -> TokenStream {
    match passing_style {
        PassingStyle::Value => quote! {#ty},
        PassingStyle::Reference => quote! {&#ty},
        PassingStyle::MutReference => quote! {& mut #ty},
    }
}

fn get_arg_pos(ident: &Ident) -> MacroResult<String> {
    let arg_string = ident.to_string();
    arg_string.split('_').nth(1).map(|x| x.to_string()).ok_or_else(|| Error::new(
        ident.span(),
        "Arg name should be in form arg_2 which means it should always have two and only two items. Internal error.",
    ))
}

/// if expecting a Vec then the actual expression itself should be iterable
/// (Pair/Vector/Nil), set arg_name_itself_is_iter = true. Otherwise it's
/// varargs which means the original args vector is passed in and
/// iterated over. the implication of passing in Vec is that if the Exp
/// doesn't match one of a few underlying sl-sh types passing it to
/// this function is an error.
fn parse_variadic_args_type(
    arg_name_itself_is_iter: bool,
    ty: &TypePath,
    fn_name: &str,
    arg_name: &Ident,
    inner: TokenStream,
    collect_type: TokenStream,
) -> MacroResult<TokenStream> {
    let rust_type = get_type_or_wrapped_type(ty, POSSIBLE_ARG_TYPES.as_slice());
    let arg_pos = get_arg_pos(arg_name)?;
    match rust_type {
        RustType::Path(wrapped_ty, _span) => {
            let arg_check = if arg_name_itself_is_iter {
                quote! {
                    let #arg_name = if !crate::is_sequence!(#arg_name)
                    {
                        let err_str = format!("{}: Expected a vector or list for argument at position {}.", #fn_name, #arg_pos);
                        return Err(LispError::new(err_str));
                    } else {
                        #arg_name
                    };
                }
            } else {
                // HACK: this should not be needed but sometimes in current sl-sh 0.9.69 implementation
                // VarArgs (which are being passed in when arg_name_itself_is_iter  is false) can be
                // passed an array that contains nil, which is different than nil itself, either way
                // this can be dealt with easily at the macro level. Removing this code causes
                // the try_into_for in the final quote! block to fail because it can't convert
                // nil into its desired type. This happens because it is iterating over a list
                // that is nil. In reality, it shouldn't but the Expression object is a vector of
                // nil, this is defensive code that should go away in slosh.
                quote! {
                    let #arg_name = if #arg_name.len() == 1 && #arg_name.get(0).unwrap().is_nil() {
                        vec![]
                    } else {
                        #arg_name
                    };
                }
            };
            Ok(quote! {{
                #arg_check
                use crate::builtins_util::TryIntoExpression;

                static_assertions::assert_impl_all!(crate::types::Expression: crate::builtins_util::TryIntoExpression<#wrapped_ty>);
                let #arg_name = #arg_name
                    .iter()
                    .map(|#arg_name| {
                        #arg_name.clone().try_into_for(#fn_name)
                    })
                    .collect::<crate::LispResult<#ty>>()?;
                #inner
            }})
        }
        RustType::Tuple(type_tuple, _span) => {
            if !type_tuple.elems.is_empty() {
                let arg_pos = get_arg_pos(arg_name)?;
                let arg_check = if arg_name_itself_is_iter {
                    quote! {
                        if !crate::is_sequence!(#arg_name)
                        {
                            let err_str = format!("{}: Expected a vector or list for argument at position {}.", #fn_name, #arg_pos);
                            return Err(LispError::new(err_str));
                        }
                    }
                } else {
                    quote! {}
                };
                let tuple_len = type_tuple.elems.len();
                let arg_name_base = arg_name.to_string() + "_";
                let arg_names = (0..type_tuple.elems.len())
                    .map(|x| {
                        Ident::new(
                            &(arg_name_base.to_string() + &x.to_string()),
                            Span::call_site(),
                        )
                    })
                    .collect::<Vec<Ident>>();
                let mut types = vec![];
                let mut type_assertions = vec![];
                let mut args = vec![];
                for (elem, arg_name) in type_tuple.elems.iter().zip(arg_names.iter()) {
                    types.push(elem.clone());
                    type_assertions.push(quote! {
                        static_assertions::assert_impl_all!(crate::types::Expression: crate::builtins_util::TryIntoExpression<#elem>);
                    });
                    args.push(quote! {
                        let #arg_name: #elem = #arg_name.clone().try_into_for(#fn_name)?;
                    })
                }
                Ok(quote! {{
                    use crate::builtins_util::TryIntoExpression;
                    use std::convert::TryInto;
                    #(#type_assertions)*
                    #arg_check
                    let #arg_name = #arg_name
                        .iter()
                        .map(|#arg_name| {
                            let #arg_name = #arg_name.iter().collect::<Vec<crate::types::Expression>>();
                            match #arg_name.try_into() {
                                Ok(#arg_name) => {
                                    let #arg_name: [crate::Expression; #tuple_len] = #arg_name;
                                    let [#(#arg_names),*] = #arg_name;
                                    #(#args)*
                                    Ok((#(#arg_names),*))
                                }
                                Err(_) => {
                                    let err_str = format!("{}: Expected a sl_sh vector or list of tuples of length {} corresponding to the argument at position {}.", #fn_name, #tuple_len, #arg_pos);
                                    Err(LispError::new(err_str))
                                }
                            }
                        })
                        .collect::<crate::LispResult<#collect_type<(#(#types),*)>>>()?;
                    #inner
                }})
            } else {
                let arg_pos = get_arg_pos(arg_name)?;
                let err_str = format!(
                    "Error with argument at position {}, sl_sh_fn only supports tuple pairs.",
                    arg_pos
                );
                Err(Error::new(type_tuple.span(), err_str))
            }
        }
        ty => {
            let err_str = "Vec<T> and VarArgs<T> only support T of type Type::Path or Type::Tuple.";
            Err(Error::new(ty.span(), err_str))
        }
    }
}

/// for Option<Expression> values the ref_exp must first be parsed as an
/// Option, and only in the case that the option is Some will it be
/// necessary to match against every ExpEnum variant.
#[allow(clippy::too_many_arguments)]
fn parse_optional_type(
    ty: &TypePath,
    fn_name: &str,
    fn_name_ident: &Ident,
    arg_name: &Ident,
    passing_style: PassingStyle,
    inner: TokenStream,
    idx: usize,
    required_args: usize,
    param: Param,
) -> MacroResult<TokenStream> {
    let some_inner = quote! {
        let #arg_name = Some(#arg_name);
        #inner
    };
    // in the case that the value is some, which means the Expression is no longer
    // wrapped in Option, the parse_typehandle_value_type can be repurposed but
    // with the caveat that after the value of inner it is handed first wraps
    // the matched ExpEnum in Some bound to the #arg_name like the
    // rust native function expects.
    let some_arg_value_type_parsing_code = parse_direct_type(
        ty,
        fn_name,
        fn_name_ident,
        arg_name,
        passing_style,
        some_inner,
        idx,
        required_args,
        param,
    )?;
    Ok(quote! {
        match #arg_name {
            None => {
                let #arg_name = None;
                #inner
            }
            Some(#arg_name) => {
               #some_arg_value_type_parsing_code
            }
        }
    })
}

/// None if not Rust type vec
fn is_vec(ty: &TypePath) -> Option<Type> {
    if let Some((ty, type_path)) = get_generic_argument_from_type_path(ty) {
        let wrapper = opt_is_valid_generic_type(type_path, &["Vec"]);
        if let (GenericArgument::Type(ty), Some(_)) = (ty, wrapper) {
            match <Type as Into<RustType>>::into(ty.clone()) {
                RustType::Path(_, _) | RustType::Tuple(_, _) => return Some(ty.clone()),
                _ => {}
            }
        }
    }
    None
}

/// at this point the macro is only operating on types it expects
/// which are any rust types, any rust types wrapped in Option,
/// and any rust types wrapped in Vec. If in the future this is
/// confusing wrapper types can be made, i.e. SlshVarArgs,
/// so normal rust Vec could be used without being turned into
/// a SlshVarArgs
fn get_type_or_wrapped_type<'a>(ty: &'a TypePath, possible_types: &'a [&str]) -> RustType {
    if let Some((ty, type_path)) = get_generic_argument_from_type_path(ty) {
        let wrapper = opt_is_valid_generic_type(type_path, possible_types);
        if let (GenericArgument::Type(ty), Some(_)) = (ty, wrapper) {
            return <Type as Into<RustType>>::into(ty.clone());
        }
    }
    RustType::Path(ty.clone(), ty.span())
}

/// for regular Expression values (no Optional/VarArgs) ref_exp
/// just needs to be matched based on it's ExpEnum variant.
#[allow(clippy::too_many_arguments)]
fn parse_direct_type(
    ty: &TypePath,
    fn_name: &str,
    fn_name_ident: &Ident,
    arg_name: &Ident,
    passing_style: PassingStyle,
    inner: TokenStream,
    idx: usize,
    required_args: usize,
    param: Param,
) -> MacroResult<TokenStream> {
    if is_vec(ty).is_some() {
        parse_variadic_args_type(true, ty, fn_name, arg_name, inner, quote! { Vec })
    } else {
        let ty = get_type_or_wrapped_type(ty, SPECIAL_ARG_TYPES.as_slice());
        match ty {
            RustType::Path(ty, _span) => {
                let str = ty.to_token_stream().to_string();
                // handle &str differently, want impl RustProcedure<F> for TypedWrapper<&str>
                // w/o this special case it generate RustProcedureRefMut on a TypedWrapper<str> which is unsized.
                let (fn_ref, passing_style, ty) =
                    if str == "str" && passing_style == PassingStyle::Reference {
                        let passing_style = PassingStyle::Value;
                        (quote! { &#ty }, passing_style, quote! { &#ty })
                    } else {
                        (
                            tokens_for_matching_references(passing_style, &ty),
                            passing_style,
                            quote! { #ty },
                        )
                    };
                let callback_declaration = quote! {
                    let callback = |#arg_name: #fn_ref| -> crate::LispResult<crate::types::Expression> {
                        #inner
                    };
                };

                match passing_style {
                    PassingStyle::Value | PassingStyle::Reference => Ok(quote! {{
                        use crate::types::RustProcedure;
                        let typed_data: crate::types::TypedWrapper<#ty, crate::types::Expression> =
                            crate::types::TypedWrapper::new(&#arg_name);
                        #callback_declaration
                        typed_data.apply(#fn_name_ident, callback)
                    }}),
                    PassingStyle::MutReference => Ok(quote! {{
                        use crate::types::RustProcedureRefMut;
                        let mut typed_data: crate::types::TypedWrapper<#ty, crate::types::Expression> =
                            crate::types::TypedWrapper::new(&#arg_name);
                        #callback_declaration
                        typed_data.apply_ref_mut(#fn_name_ident, callback)
                    }}),
                }
            }
            RustType::Tuple(type_tuple, _span) => parse_type_tuple(
                &type_tuple,
                fn_name,
                fn_name_ident,
                inner,
                arg_name,
                idx,
                required_args,
                param,
                no_parse_param,
            ),
            RustType::BareFn(_, _) | RustType::Reference(_, _) | RustType::Unsupported(_) => {
                let arg_pos = get_arg_pos(arg_name)?;
                let err_str = format!(
                    "Error with argument at position {}, sl_sh_fn only supports Vec<T>, Option<T>, and T where T is a Type::Path or Type::Tuple and can be moved, passed by reference, or passed by mutable reference (|&|&mut )(Type Path | (Type Path,*))",
                    arg_pos
                );
                Err(Error::new(ty.span(), err_str))
            }
        }
    }
}

/// create the nested match statements to parse rust types into sl_sh types.
/// the rust types will determine what sl_sh functions will be used for
/// transformation. If this function throws errors it means that the
/// inputs, val/passing style are wrong and aren't matching to the ArgType(s)
/// properly, or the rust type lookup function is busted.
#[allow(clippy::too_many_arguments)]
fn parse_type(
    ty: &TypePath,
    fn_name: (&str, &Ident),
    inner: TokenStream,
    param: Param,
    arg_name: &Ident,
    passing_style: PassingStyle,
    idx: usize,
    required_args: usize,
    outer_parse: fn(&Ident, TokenStream, Param, usize, usize) -> TokenStream,
) -> MacroResult<TokenStream> {
    let tokens = match param.handle {
        TypeHandle::Direct => parse_direct_type(
            ty,
            fn_name.0,
            fn_name.1,
            arg_name,
            passing_style,
            inner,
            idx,
            required_args,
            param,
        )?,
        TypeHandle::Optional => parse_optional_type(
            ty,
            fn_name.0,
            fn_name.1,
            arg_name,
            passing_style,
            inner,
            idx,
            required_args,
            param,
        )?,
        TypeHandle::VarArgs => parse_variadic_args_type(
            false,
            ty,
            fn_name.0,
            arg_name,
            inner,
            quote! { crate::VarArgs },
        )?,
    };
    Ok(outer_parse(arg_name, tokens, param, required_args, idx))
}

/// create a vec literal of the expected Param types so code can check its arguments at runtime for
/// API arity/type correctness.
fn embed_params_vec(params: &[Param]) -> TokenStream {
    let mut tokens = vec![];
    for param in params {
        tokens.push(match (param.handle, param.passing_style) {
            (TypeHandle::Direct, PassingStyle::MutReference) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::Direct,
                    passing_style: crate::builtins_util::PassingStyle::MutReference
                }}
            }
            (TypeHandle::Optional, PassingStyle::MutReference) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::Optional,
                    passing_style: crate::builtins_util::PassingStyle::MutReference
                }}
            }
            (TypeHandle::VarArgs, PassingStyle::MutReference) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::VarArgs,
                    passing_style: crate::builtins_util::PassingStyle::MutReference
                }}
            }
            (TypeHandle::Direct, PassingStyle::Reference) => {
                quote! {crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::Direct,
                    passing_style: crate::builtins_util::PassingStyle::Reference
                }}
            }
            (TypeHandle::Optional, PassingStyle::Reference) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::Optional,
                    passing_style: crate::builtins_util::PassingStyle::Reference
                }}
            }
            (TypeHandle::VarArgs, PassingStyle::Reference) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::VarArgs,
                    passing_style: crate::builtins_util::PassingStyle::Reference
                }}
            }
            (TypeHandle::Direct, PassingStyle::Value) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::Direct,
                    passing_style: crate::builtins_util::PassingStyle::Value
                }}
            }
            (TypeHandle::Optional, PassingStyle::Value) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::Optional,
                    passing_style: crate::builtins_util::PassingStyle::Value
                }}
            }
            (TypeHandle::VarArgs, PassingStyle::Value) => {
                quote! { crate::builtins_util::Param {
                    handle: crate::builtins_util::TypeHandle::VarArgs,
                    passing_style: crate::builtins_util::PassingStyle::Value
                }}
            }
        });
    }
    let const_params_len = get_const_params_len_ident();
    quote! {
        let arg_types: [crate::builtins_util::Param; #const_params_len] = [ #(#tokens),* ];
    }
}

/// write the intern_ function code. This code is generated to be called within sl-sh to avoid writing
/// boilerplate code to submit a function symbol and the associated code to the runtime. Every builtin
/// function must be inserted into a hashmap where the key is the name of the function and the value
/// is a function expression that stores the name of the rust function to call and its documentation.
/// It looks like the following in all cases:
// ```
// fn intern_one_int_to_float<S: std::hash::BuildHasher>(
//    interner: &mut sl_sh::Interner,
//    data: &mut std::collections::HashMap<&'static str, (sl_sh::types::Expression, String), S>,
//) {
//    let fn_name = "oneintofloat";
//    data.insert(
//        interner.intern(fn_name),
//        sl_sh::types::Expression::make_function(parse_one_int_to_float, " my docs\n"),
//    );
//}
// ```
fn generate_intern_fn(
    original_fn_name_str: &str,
    fn_name_ident: &Ident,
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
            let #fn_name_ident = #fn_name;
            data.insert(
                interner.intern(#fn_name_ident),
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
fn generate_parse_fn(
    original_fn_name_str: &str,
    eval_values: bool,
    fn_name_ident: &Ident,
    fn_name: &str,
    args_len: usize,
    params: &[Param],
    inner: TokenStream,
) -> TokenStream {
    let parse_name = get_parse_fn_name(original_fn_name_str);
    let arg_vec_literal = embed_params_vec(params);

    // in slosh this will change because the args are already evaluated and the macro will
    // be dealing with a slice so... keep this allocation at runtime for now because it
    // simplified the implementation and is more realistic long-term even though it's
    // suboptimal in this case.
    let make_args = if eval_values {
        quote! {
            let args = crate::builtins_util::make_args(environment, args)?;
            let args = args.into_iter().collect::<Vec<Expression>>();
            let args = args.as_slice();

        }
    } else {
        quote! {
            let args = crate::builtins_util::make_args_eval_no_values(environment, args)?;
            let args = args.into_iter().collect::<Vec<Expression>>();
            let args = args.as_slice();
        }
    };

    let const_params_len = get_const_params_len_ident();
    quote! {
        fn #parse_name(
            environment: &mut crate::environment::Environment,
            args: &mut dyn Iterator<Item = crate::types::Expression>,
        ) -> crate::LispResult<crate::types::Expression> {
            #make_args
            let #fn_name_ident = #fn_name;
            const #const_params_len: usize = #args_len;
            #arg_vec_literal

            #inner
        }
    }
}

fn num_required_args(params: &[Param]) -> usize {
    params.iter().fold(0, |accum, nxt| {
        if nxt.handle == TypeHandle::Direct {
            accum + 1
        } else {
            accum
        }
    })
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
fn generate_builtin_fn(
    original_item_fn: &ItemFn,
    original_fn_name_str: &str,
    fn_name: &str,
    params: &[Param],
    fn_name_ident: &Ident,
    takes_env: bool,
    inline: bool,
) -> MacroResult<TokenStream> {
    let original_fn_name = Ident::new(original_fn_name_str, Span::call_site());
    let arg_names = generate_inner_fn_signature_to_orig_fn_call(original_item_fn, takes_env)?;
    let required_args = num_required_args(params);

    let orig_fn_call = make_orig_fn_call(
        inline,
        takes_env,
        original_item_fn,
        &original_fn_name,
        required_args,
        arg_names.clone(),
    )?;
    // initialize the innermost token stream to the code of the original_fn_call
    let mut prev_token_stream = orig_fn_call;
    let skip = usize::from(takes_env);
    let inputs_less_env_len = original_item_fn.sig.inputs.len() - skip;
    if inputs_less_env_len != params.len() {
        let err_str = format!(
            "sl_sh_fn macro is broken, signature of target function has an arity of {}, but this macro computed its arity as: {} (arity is - 1 if takes_env is true).",
            inputs_less_env_len,
            params.len(),
        );
        return Err(Error::new(original_item_fn.span(), err_str));
    }
    let fn_args = original_item_fn
        .sig
        .inputs
        .iter()
        .skip(skip)
        .zip(arg_names.iter())
        .zip(params.iter());
    for (idx, ((fn_arg, arg_name), param)) in fn_args.enumerate() {
        if let FnArg::Typed(ty) = fn_arg {
            // this needs to use the args.iter() pattern now.
            prev_token_stream = parse_fn_arg_type(
                &ty.ty,
                fn_name,
                fn_name_ident,
                prev_token_stream,
                arg_name,
                false,
                idx,
                *param,
                required_args,
            )?;
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
        #(#conversions_assertions_code)*
        // let #fn_name_ident = #fn_name; already included in parse function
        // as well as `args`, a vec of expression,
        // `fn_name_ident` the ident of the fn_name
        // `ARGS_LEN` constant representing arity of original fn
        // and `arg_types` the embedded vec of Arg's available at runtime.
        #prev_token_stream
    };
    Ok(tokens)
}

/// recursively wrap the received sl_sh args at the given idx in callback functions that parse the Expressions
/// into the a variable with a predefined  name with the same type as the corresponding parameter
/// in the rust native function.
#[allow(clippy::too_many_arguments)]
fn parse_fn_arg_type(
    ty: &Type,
    fn_name: &str,
    fn_name_ident: &Ident,
    prev_token_stream: TokenStream,
    arg_name: &Ident,
    noop_outer_parse: bool,
    idx: usize,
    param: Param,
    required_args: usize,
) -> MacroResult<TokenStream> {
    match <Type as Into<RustType>>::into(ty.clone()) {
        RustType::Path(ty, _span) => {
            let parse_layer_1 = get_parser_for_type_handle(noop_outer_parse);
            let passing_style = PassingStyle::Value;
            parse_type(
                &ty,
                (fn_name, fn_name_ident),
                prev_token_stream,
                param,
                arg_name,
                passing_style,
                idx,
                required_args,
                parse_layer_1,
            )
        }
        RustType::Tuple(type_tuple, _span) => {
            let parse_layer_1 = get_parser_for_type_handle(noop_outer_parse);
            parse_type_tuple(
                &type_tuple,
                fn_name,
                fn_name_ident,
                prev_token_stream,
                arg_name,
                idx,
                required_args,
                param,
                parse_layer_1,
            )
        }
        RustType::Reference(ty_ref, _span) => match <Type as Into<RustType>>::into(*ty_ref.elem) {
            RustType::Path(ty, _span) => {
                let parse_layer_1 = get_parser_for_type_handle(noop_outer_parse);
                let passing_style = if ty_ref.mutability.is_some() {
                    PassingStyle::MutReference
                } else {
                    PassingStyle::Reference
                };
                parse_type(
                    &ty,
                    (fn_name, fn_name_ident),
                    prev_token_stream,
                    param,
                    arg_name,
                    passing_style,
                    idx,
                    required_args,
                    parse_layer_1,
                )
            }
            RustType::Tuple(type_tuple, _span) => {
                let parse_layer_1 = get_parser_for_type_handle(noop_outer_parse);
                parse_type_tuple(
                    &type_tuple,
                    fn_name,
                    fn_name_ident,
                    prev_token_stream,
                    arg_name,
                    idx,
                    required_args,
                    param,
                    parse_layer_1,
                )
            }
            RustType::BareFn(_, _) | RustType::Unsupported(_) | RustType::Reference(_, _) => {
                let arg_pos = get_arg_pos(arg_name)?;
                let err_str = format!(
                    "Error with argument at position {}, sl_sh_fn only supports Vec<T>, Option<T>, and T where T is a Type::Path or Type::Tuple and can be moved, passed by reference, or passed by mutable reference (|&|&mut )(Type Path | (Type Path,*))",
                    arg_pos
                );
                Err(Error::new(ty.span(), err_str))
            }
        },
        RustType::BareFn(_, _) | RustType::Unsupported(_) => {
            let arg_pos = get_arg_pos(arg_name)?;
            let err_str = format!(
                "Error with argument at position {}, sl_sh_fn only supports Vec<T>, Option<T>, and T where T is a Type::Path or Type::Tuple and can be moved, passed by reference, or passed by mutable reference (|&|&mut )(Type Path | (Type Path,*))",
                arg_pos
            );
            Err(Error::new(ty.span(), err_str))
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn parse_type_tuple(
    type_tuple: &TypeTuple,
    fn_name: &str,
    fn_name_ident: &Ident,
    inner: TokenStream,
    arg_name: &Ident,
    idx: usize,
    required_args: usize,
    param: Param,
    outer_parse: fn(&Ident, TokenStream, Param, usize, usize) -> TokenStream,
) -> MacroResult<TokenStream> {
    // at the end of all the tuple parsing the inner token stream expects
    // arg_name to be:
    // let arg_name_N: (T, U) = (arg_name_N_0, arg_name_N_1);
    // this means that first we must take the arg_names of what will be the
    // tuple pair and put them back into the ident that this recursive process
    // expects.
    let arg_name_base = arg_name.to_string() + "_";
    let arg_names = (0..type_tuple.elems.len())
        .map(|x| {
            Ident::new(
                &(arg_name_base.to_string() + &x.to_string()),
                Span::call_site(),
            )
        })
        .collect::<Vec<Ident>>();
    let mut inner = quote! {
        let #arg_name = (#(#arg_names),*);
        #inner
    };
    let mut expressions = vec![];
    let tuple_len = type_tuple.elems.len();
    let tokens = if !type_tuple.elems.is_empty() {
        for (i, ty) in type_tuple.elems.iter().enumerate() {
            expressions.push(quote! { crate::types::Expression });
            let arg_name_pair = Ident::new(
                &(arg_name_base.to_string() + &i.to_string()),
                Span::call_site(),
            );
            let param = get_param_from_type(ty.clone(), ty.span(), i)?;
            inner = parse_fn_arg_type(
                ty,
                fn_name,
                fn_name_ident,
                inner,
                &arg_name_pair,
                true,
                i,
                param,
                required_args,
            )?;
        }
        inner
    } else {
        inner
    };
    let arg_pos = get_arg_pos(arg_name)?;
    let tokens = quote! {{
        use std::convert::TryInto;
        if !crate::is_sequence!(#arg_name)
        {
            let err_str = format!("{}: Expected a vector or list for argument at position {}.", #fn_name, #arg_pos);
            return Err(crate::types::LispError::new(err_str));
        }
        let #arg_name = #arg_name.iter().collect::<Vec<crate::types::Expression>>();
        match #arg_name.try_into() {
            Ok(#arg_name) => {
                let #arg_name: [crate::Expression; #tuple_len] = #arg_name;
                let [#(#arg_names),*] = #arg_name;
                #tokens
            }
            Err(_) => {
                let err_str = format!("{}: Expected a sl_sh vector or list with {} elements corresponding to the tuple at argument position {}.", #fn_name, #tuple_len, #arg_pos);
                return Err(crate::types::LispError::new(err_str));
            }
        }
    }};
    Ok(outer_parse(arg_name, tokens, param, required_args, idx))
}

/// Optional and VarArgs types are supported to create the idea of items that might be provided or
/// for providing a list of zero or more items that can be passed in.
/// The nature of optional and varargs are context dependent because variable numbers of
/// arguments have to be at the end of the function signature. This method verifies that items
/// marked as Optional are last, and VarArgs is supported but only in the last position, which can
/// be after any number of Optional arguments. This means non Optional/VarArgs types must
/// come before all Optional and VarArgs types.
fn are_args_valid(original_item_fn: &ItemFn, params: &[Param], takes_env: bool) -> MacroResult<()> {
    if params.is_empty() || (!takes_env && params.len() == 1 || takes_env && params.len() == 2) {
        Ok(())
    } else {
        let mut found_opt = false;
        let mut found_value = false;
        for (i, param) in params.iter().rev().enumerate() {
            match (i, param.handle, found_opt, found_value) {
                (i, TypeHandle::VarArgs, _, _) if i > 0 => {
                    return Err(Error::new(
                        original_item_fn.span(),
                        "Only one Vec argument is supported and it must be the last argument.",
                    ));
                }
                (_, TypeHandle::Optional, _, true) => {
                    return Err(Error::new(
                        original_item_fn.span(),
                        "Optional argument(s) must be placed last.",
                    ));
                }
                (_, TypeHandle::Optional, _, _) => {
                    found_opt = true;
                }
                (_, TypeHandle::Direct, _, _) => {
                    found_value = true;
                }
                (_, _, _, _) => {}
            }
        }
        Ok(())
    }
}

fn get_param_from_type(ty: Type, span: Span, pos: usize) -> MacroResult<Param> {
    let ty_clone = ty.clone();
    let param = match <Type as Into<RustType>>::into(ty) {
        RustType::Path(ty, _span) => {
            let val = get_type_handle(&ty);
            Param {
                handle: val,
                passing_style: PassingStyle::Value,
            }
        }
        RustType::Tuple(_type_tuple, _span) => Param {
            handle: TypeHandle::Direct,
            passing_style: PassingStyle::Value,
        },
        RustType::Reference(ty_ref, _span) => {
            let passing_style = if ty_ref.mutability.is_some() {
                PassingStyle::MutReference
            } else {
                PassingStyle::Reference
            };
            match <Type as Into<RustType>>::into(*ty_ref.elem) {
                RustType::Path(ty, _span) => {
                    let val = get_type_handle(&ty);
                    Param {
                        handle: val,
                        passing_style,
                    }
                }
                RustType::Tuple(_type_tuple, _span) => Param {
                    handle: TypeHandle::Direct,
                    passing_style,
                },
                _ => {
                    return Err(Error::new(
                        span,
                        format!(
                            "Error with argument at position {}, sl_sh_fn only supports passing Type::Path and Type::Tuple by value or ref/ref mut, no either syn::Type's are supported: {:?}.",
                            pos,
                            ty_clone.to_token_stream(),
                        ),
                    ));
                }
            }
        }
        _ => {
            return Err(Error::new(
                span,
                format!(
                    "Error with argument at position {}, sl_sh_fn only supports passing Type::Path and Type::Tuple by value or ref/ref mut, no either syn::Type's are supported: {:?}.",
                    pos,
                    ty_clone.to_token_stream(),
                ),
            ));
        }
    };
    Ok(param)
}

/// Create a Vec<Arg> from the original fn's signature. Information is needed at compile and
/// run time to translate the list of sl_sh expressions to rust native types. This Arg types
/// stores the information about the rust native type (Value/Option/Var) as well as whether it's moved, passed
/// by reference, or passed by mutable reference.
fn parse_src_function_arguments(
    original_item_fn: &ItemFn,
    takes_env: bool,
) -> MacroResult<Vec<Param>> {
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

    let skip = usize::from(takes_env);

    for (i, fn_arg) in original_item_fn.sig.inputs.iter().enumerate().skip(skip) {
        match fn_arg {
            FnArg::Receiver(_) => {
                return Err(Error::new(
                    original_item_fn.span(),
                    "Associated functions that take the self argument are not supported.",
                ))
            }
            FnArg::Typed(ty) => {
                parsed_args.push(get_param_from_type(
                    *ty.ty.clone(),
                    original_item_fn.span(),
                    i,
                )?);
            }
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

fn get_const_params_len_ident() -> Ident {
    Ident::new("PARAMS_LEN", Span::call_site())
}

fn parse_attributes(
    original_item_fn: &ItemFn,
    attr_args: AttributeArgs,
) -> MacroResult<(String, Ident, bool, bool, bool)> {
    let vals = attr_args
        .iter()
        .map(get_attribute_name_value)
        .collect::<MacroResult<Vec<(String, String)>>>()?;
    let fn_name_ident = "fn_name".to_string();
    let fn_name = get_attribute_value_with_key(original_item_fn, &fn_name_ident, vals.as_slice())?
        .ok_or_else(|| {
            Error::new(
                original_item_fn.span(),
                "sl_sh_fn requires name-value pair, 'fn_name'",
            )
        })?;
    let fn_name_ident = Ident::new(&fn_name_ident, Span::call_site());

    let eval_values =
        get_bool_attribute_value_with_key(original_item_fn, "eval_values", vals.as_slice())?;
    let takes_env =
        get_bool_attribute_value_with_key(original_item_fn, "takes_env", vals.as_slice())?;

    // all functions default to inlining unless explicitly overriden.
    let inline =
        !get_bool_attribute_value_with_key(original_item_fn, "do_not_inline", vals.as_slice())?;

    Ok((fn_name, fn_name_ident, eval_values, takes_env, inline))
}

/// this function outputs all of the generated code, it is composed into two different functions:
/// intern_<original_fn_name>
/// parse_<original_fn_name>
/// - intern_ is the simplest function, it is generated to be called within sl-sh to avoid writing
/// boilerplate code to submit a function symbol and the associated code to the runtime.
/// - parse_ has the same function signature as all rust native functions, it takes the environment
/// and a list of args. It evals those arguments at runtime and converts them to rust types
/// they can be consumed by the target rust function.
fn generate_sl_sh_fn(
    original_item_fn: &ItemFn,
    attr_args: AttributeArgs,
) -> MacroResult<TokenStream> {
    let (fn_name, fn_name_ident, eval_values, takes_env, inline) =
        parse_attributes(original_item_fn, attr_args)?;
    let original_fn_name_str = original_item_fn.sig.ident.to_string();
    let original_fn_name_str = original_fn_name_str.as_str();

    let params = parse_src_function_arguments(original_item_fn, takes_env)?;
    are_args_valid(original_item_fn, params.as_slice(), takes_env)?;
    let builtin_fn = generate_builtin_fn(
        original_item_fn,
        original_fn_name_str,
        fn_name.as_str(),
        params.as_slice(),
        &fn_name_ident,
        takes_env,
        inline,
    )?;

    let args_len = if takes_env {
        original_item_fn.sig.inputs.len() - 1
    } else {
        original_item_fn.sig.inputs.len()
    };
    let parse_fn = generate_parse_fn(
        original_fn_name_str,
        eval_values,
        &fn_name_ident,
        fn_name.as_str(),
        args_len,
        params.as_slice(),
        builtin_fn,
    );
    let doc_comments = get_documentation_for_fn(original_item_fn)?;
    let intern_fn = generate_intern_fn(
        original_fn_name_str,
        &fn_name_ident,
        fn_name.as_str(),
        doc_comments,
    );
    let tokens = quote! {
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

                    #[allow(dead_code)]
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

//TODO
//  - functions that return Values, tuple return types?
//  - fcns that accept iters?
//  - then... compare against inline the function being called... randomize variable names...
//      and fn names too? could pick some random string and prefix all generated idents.

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    // serves as a model for what it's like at runtime to iterate over the parameters of a function,
    // T serves as a generic so these tests can run with some data, but in practice T is some
    // type from the consuming library.
    fn loop_over_to_inline<T, const N: usize>(
        fn_name: &str,
        params: &[Param; N],
        args: &[T],
    ) -> Result<(), String> {
        let required_args = num_required_args(params);
        for idx in 0..N {
            to_inline(fn_name, idx, required_args, params, args)?;
        }
        if N > 0 {
            too_many_args_detection(fn_name, params, N, args)?;
        }

        Ok(())
    }

    // run last to see if the number of received arguments has exceeded the number expected based
    // on the arity of the rust function, and whether or not it ends in a varargs/array.
    fn too_many_args_detection<T>(
        fn_name: &str,
        arg_types: &[Param],
        len: usize,
        args: &[T],
    ) -> Result<(), String> {
        match args.get(len) {
            Some(_) if arg_types[len - 1].handle != TypeHandle::VarArgs => {
                return Err(format!(
                    "{} given too many arguments, expected {}, got {}.",
                    fn_name,
                    arg_types.len(),
                    args.len()
                ));
            }
            _ => {
                //macro
                println!("macro")
            }
        }
        Ok(())
    }

    // loop over each input and check based on current idx and presence of Arg or not
    // whether the args received is lower than needed based on the arity of the rust function.
    fn to_inline<T>(
        fn_name: &str,
        idx: usize,
        required_args: usize,
        params: &[Param],
        args: &[T],
    ) -> Result<(), String> {
        let param = params[idx];
        match args.get(idx) {
            None if param.handle == TypeHandle::Direct => {
                return Err(format!(
                    "{} not given enough arguments, expected at least {} arguments, got {}.",
                    fn_name,
                    required_args,
                    args.len()
                ));
            }
            _arg => {
                // insert
                println!("macro");
            }
        }
        Ok(())
    }
    struct Foo {}

    #[test]
    fn test_params_values_only() {
        let two_moved_values = vec![
            Param {
                handle: TypeHandle::Direct,
                passing_style: PassingStyle::Value,
            },
            Param {
                handle: TypeHandle::Direct,
                passing_style: PassingStyle::Value,
            },
        ];

        // if there are not enough arguments we throw an error.
        let args = vec![Foo {}];
        let args = loop_over_to_inline::<Foo, 2>(
            "foo",
            two_moved_values.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("not given enough arguments"));

        // if there are too many arguments we throw an error.
        let args = vec![Foo {}, Foo {}, Foo {}];
        let args = loop_over_to_inline::<Foo, 2>(
            "foo",
            two_moved_values.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("given too many"));

        let args = vec![Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 2>(
            "foo",
            two_moved_values.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
    }

    #[test]
    fn test_params_optionals() {
        let one_val_one_opt = vec![
            Param {
                handle: TypeHandle::Direct,
                passing_style: PassingStyle::Value,
            },
            Param {
                handle: TypeHandle::Optional,
                passing_style: PassingStyle::Value,
            },
        ];
        let args = vec![Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 2>(
            "foo",
            one_val_one_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![Foo {}];
        loop_over_to_inline::<Foo, 2>(
            "foo",
            one_val_one_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![];
        let args = loop_over_to_inline::<Foo, 2>(
            "foo",
            one_val_one_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("not given enough arguments"));

        let args = vec![Foo {}, Foo {}, Foo {}];
        let args = loop_over_to_inline::<Foo, 2>(
            "foo",
            one_val_one_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("given too many"));

        let val_and_opt = vec![
            Param {
                handle: TypeHandle::Direct,
                passing_style: PassingStyle::Value,
            },
            Param {
                handle: TypeHandle::Optional,
                passing_style: PassingStyle::Value,
            },
        ];
        let args = vec![Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 2>(
            "foo",
            val_and_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![Foo {}];
        loop_over_to_inline::<Foo, 2>(
            "foo",
            val_and_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![];
        let args = loop_over_to_inline::<Foo, 2>(
            "foo",
            val_and_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("not given enough arguments"));

        let args = vec![Foo {}, Foo {}, Foo {}];
        let args = loop_over_to_inline::<Foo, 2>(
            "foo",
            val_and_opt.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("given too many"));
    }

    #[test]
    fn test_params_vec() {
        let one_vec = vec![Param {
            handle: TypeHandle::VarArgs,
            passing_style: PassingStyle::MutReference,
        }];

        let args = vec![];
        loop_over_to_inline::<Foo, 1>(
            "foo",
            one_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![Foo {}];
        loop_over_to_inline::<Foo, 1>(
            "foo",
            one_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 1>(
            "foo",
            one_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let args = vec![Foo {}, Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 1>(
            "foo",
            one_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
    }

    #[test]
    fn test_params_vec_with_options() {
        let val_opt_and_vec = vec![
            Param {
                handle: TypeHandle::Direct,
                passing_style: PassingStyle::Reference,
            },
            Param {
                handle: TypeHandle::Optional,
                passing_style: PassingStyle::MutReference,
            },
            Param {
                handle: TypeHandle::VarArgs,
                passing_style: PassingStyle::Value,
            },
        ];

        let args = vec![];
        let args = loop_over_to_inline::<Foo, 3>(
            "foo",
            val_opt_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        );
        assert!(args.unwrap_err().contains("not given enough arguments"));
        let args = vec![Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            val_opt_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            val_opt_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}, Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            val_opt_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}, Foo {}, Foo {}, Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            val_opt_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");

        let opts_and_vec = vec![
            Param {
                handle: TypeHandle::Optional,
                passing_style: PassingStyle::Reference,
            },
            Param {
                handle: TypeHandle::Optional,
                passing_style: PassingStyle::MutReference,
            },
            Param {
                handle: TypeHandle::VarArgs,
                passing_style: PassingStyle::Value,
            },
        ];

        let args = vec![];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            opts_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            opts_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            opts_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}, Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            opts_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
        let args = vec![Foo {}, Foo {}, Foo {}, Foo {}, Foo {}];
        loop_over_to_inline::<Foo, 3>(
            "foo",
            opts_and_vec.as_slice().try_into().unwrap(),
            args.as_slice(),
        )
        .expect("Parsing should succeed.");
    }
}
