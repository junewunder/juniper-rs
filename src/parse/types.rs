use crate::annotate::*;
use crate::data::{Defn, Expr, Type};
use crate::lex::{
    self, take_ident, ttag,
    Token::{self, *},
    TokenBuffer,
};
use crate::parse::{expr::p_expr, shared::*};
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

pub fn p_type(input: TokenBuffer) -> TypeIResult {
    alt((
        p_fn,
        p_terminal,
    ))(input)
}

pub fn p_terminal(input: TokenBuffer) -> TypeIResult {
    alt((
        map(ttag(&T_TY_NUM), |_| Type::NumT),
        map(ttag(&T_TY_BOOL), |_| Type::BoolT),
        map(ttag(&T_TY_ANY), |_| Type::AnyT),
        map(ttag(&T_TY_UNIT), |_| Type::UnitT),
        map(ttag(&T_TY_STRING), |_| Type::StringT),
        map(take_ident, |x| Type::TypeVar(x)),
        p_unit,
        p_parens,
        p_mut,
    ))(input)
}

fn p_unit(input: TokenBuffer) -> TypeIResult {
    let (input, _) = ttag(&T_OP_PAREN)(input)?;
    let (input, _) = ttag(&T_CL_PAREN)(input)?;
    Ok((input, Type::UnitT))
}


fn p_parens(input: TokenBuffer) -> TypeIResult {
    let (input, _) = ttag(&T_OP_PAREN)(input)?;
    let (input, t) = p_type(input)?;
    let (input, _) = ttag(&T_CL_PAREN)(input)?;
    Ok((input, t))
}

fn p_mut(input: TokenBuffer) -> TypeIResult {
    let (input, _) = ttag(&T_MUT)(input)?;
    let (input, t) = p_terminal(input)?;
    Ok((input, Type::MutT(Box::new(t))))
}

// TODO this isn't the mooost effecient but it works
fn p_fn(input: TokenBuffer) -> TypeIResult {
    let (input, i) = p_terminal(input)?;
    let (input, _) = ttag(&T_THIN_ARROW_R)(input)?;
    let (input, o) = p_type(input)?;
    Ok((input, Type::CloT(Box::new(i), Box::new(o))))
}
