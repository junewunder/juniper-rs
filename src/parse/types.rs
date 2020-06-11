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
        map(ttag(&T_TY_NUM), |_| Type::NumT),
        map(ttag(&T_TY_BOOL), |_| Type::BoolT),
        map(ttag(&T_TY_ANY), |_| Type::AnyT),
        map(ttag(&T_TY_UNIT), |_| Type::UnitT),
        map(ttag(&T_TY_STRING), |_| Type::StringT),
        map(take_ident, |x| Type::TypeVar(x)),
        p_mut,
        // p_fn,
    ))(input)
}

fn p_mut(input: TokenBuffer) -> TypeIResult {
    let (input, _) = ttag(&T_MUT)(input)?;
    let (input, t) = p_type(input)?;
    Ok((input, Type::MutT(Box::new(t))))
}

// fn p_fn(input: TokenBuffer) -> TypeIResult {
//     let (input, _) = ttag(&T_FN)(input)?;
//     let (input, t) = p_type(input)?;
//     Ok((input, Type::MutT(Box::new(t))))
// }
