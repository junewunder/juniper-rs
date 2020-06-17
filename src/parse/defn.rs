use crate::annotate::*;
use crate::data::{Defn, Expr, Type};
use crate::lex::{take_ident, take_string, ttag, TokenBuffer};
use crate::parse::{expr::p_expr, shared::*, types::p_type};
use crate::typecheck::wrap_generics_defn;
use nom::{
    self, IResult,
    branch::alt, combinator::{map, opt},
    multi::{many0, separated_list, separated_nonempty_list},
};

pub fn p_defs(input: TokenBuffer) -> IResult<TokenBuffer, Vec<Box<Annotated<Defn>>>> {
    let (input, defs) = many0(annotated_terminal(alt((
        p_tl_var, p_prim, p_struct, p_enum, p_import,
    ))))(input)?;
    Ok((input, defs))
}

fn p_import(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_IMPORT)(input)?;
    let (input, path) = take_string(input)?;
    Ok((input, Defn::ImportD(path)))
}

pub fn p_tl_var(input: TokenBuffer) -> DefnIResult {
    let (input, name) = take_ident(input)?;
    let (input, t_params) = many0(take_ident)(input)?;
    let (input, _) = ttag(&T_COLON)(input)?;
    let (input, ty) = p_type(input)?;
    let (input, _) = ttag(&T_SEMICOLON)(input)?;
    let (input, _) = take_ident(input)?; //TODO make sure this is the same as name
    let (input, args) = many0(p_fn_arg)(input)?;
    let (input, _) = ttag(&T_EQ)(input)?;
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, body) = p_expr(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;
    Ok((input, Defn::VarD(name, args, wrap_generics_defn(ty, t_params), body)))
}

fn p_fn_arg(input: TokenBuffer) -> IResult<TokenBuffer, String> {
    alt((
        map(p_unit_arg, |_| String::from("_")),
        take_ident,
    ))(input)
}

pub fn p_prim(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_PRIM)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, xs) = separated_nonempty_list(ttag(&T_COMMA), take_ident)(input)?;
    Ok((input, Defn::PrimD(name, xs)))
}

pub fn p_struct(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_STRUCT)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, t_args) = many0(take_ident)(input)?;
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, fields) = separated_list(ttag(&T_COMMA), p_var_type_pair)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;
    Ok((input, Defn::StructD(name, t_args, fields.into())))
}

pub fn p_enum(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_ENUM)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, t_args) = many0(take_ident)(input)?;
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, variants) = separated_list(ttag(&T_COMMA), p_enum_field)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;
    Ok((input, Defn::EnumD(name, t_args, variants.into())))
}

fn p_enum_field(input: TokenBuffer) -> IResult<TokenBuffer, (String, Vec<Type>)> {
    let (input, name) = take_ident(input)?;
    let (input, ts) = opt(|input| {
        let (input, _) = ttag(&T_OP_PAREN)(input)?;
        let (input, ts): (_, Vec<Type>) = separated_list(ttag(&T_COMMA), p_type)(input)?;
        let (input, _) = opt(ttag(&T_COMMA))(input)?;
        let (input, _) = ttag(&T_CL_PAREN)(input)?;
        Ok((input, ts))
    })(input)?;
    Ok((input, (name, ts.unwrap_or(Vec::with_capacity(0)))))
}
