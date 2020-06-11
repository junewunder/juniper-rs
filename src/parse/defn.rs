use crate::annotate::*;
use crate::data::{Defn, Expr, Type};
use crate::lex::{
    self, take_ident, ttag,
    Token::{self, *},
    TokenBuffer,
};
use crate::parse::{expr::p_expr, shared::*, types::p_type};
use core::fmt::Error;
use nom::{
    self,
    branch::alt,
    bytes::complete::take_until,
    character::complete::{alpha1, char, space0, space1},
    combinator::{complete, not, peek},
    combinator::{map, opt},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, many0, many1, separated_list, separated_nonempty_list},
    sequence::delimited,
    sequence::pair,
    Err, IResult,
};
use std::collections::HashMap;
use std::rc::Rc;

pub fn p_defs(input: TokenBuffer) -> IResult<TokenBuffer, Vec<Box<Annotated<Defn>>>> {
    let (input, defs) = many0(annotated_terminal(alt((
        p_fn_named, p_prim, p_struct, p_enum,
    ))))(input)?;

    Ok((input, defs))
}

pub fn p_fn_named(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_FN)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, (x, t)) = p_fn_arg(input)?;
    let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
    let (input, body) = p_expr(input)?;

    Ok((
        input,
        Defn::FnD(name, x, t, body),
    ))
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
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, fields) = separated_list(ttag(&T_COMMA), p_var_type_pair)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;

    Ok((input, Defn::StructD(name, fields.into())))
}

pub fn p_enum(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_ENUM)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, variants) = separated_list(ttag(&T_COMMA), p_enum_field)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;

    Ok((input, Defn::EnumD(name, variants.into())))
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
