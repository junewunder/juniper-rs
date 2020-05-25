use crate::annotate::Annotated;
use crate::data::{Defn, Expr};
use crate::lex::{
    self, take_ident, ttag,
    Token::{self, *},
    TokenBuffer,
};
use crate::parse::types::*;
use crate::parse::{expr::p_expr, types::*};
use core::fmt::Error;
use nom::{
    self,
    branch::alt,
    bytes::complete::take_until,
    character::complete::{alpha1, char, space0, space1},
    combinator::map,
    combinator::{complete, not, peek},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, many0, many1, separated_nonempty_list},
    sequence::delimited,
    sequence::pair,
    Err, IResult,
};
use std::collections::HashMap;
use std::rc::Rc;

pub fn p_defs(input: TokenBuffer) -> IResult<TokenBuffer, Vec<Annotated<Defn>>> {
    let (input, defs) = many0(alt((p_fn_named, p_prim)))(input)?;

    Ok((
        input,
        defs.into_iter().fold(Vec::new(), |mut defs, def| {
            defs.push(def);
            defs
        }),
    ))
}

pub fn p_fn_named(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_FN)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, mut xs) = many1(|input| {
        let (input, x) = take_ident(input)?;
        let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
        Ok((input, x))
    })(input)?;
    let (input, body) = p_expr(input)?;
    let x_top = xs.remove(0);
    let xs: Vec<_> = xs.iter().rev().collect();

    Ok((
        input,
        Annotated {
            tok: Defn::FnD(
                name,
                x_top,
                xs.into_iter().fold(body, |acc, x| box Annotated {
                    tok: Expr::FnE(None, x.clone(), acc),
                    idx: 0,
                    len: 0,
                }),
            ),
            idx: 0,
            len: 0,
        },
    ))
}

pub fn p_prim(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_PRIM)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, xs) = separated_nonempty_list(ttag(&T_COMMA), take_ident)(input)?;

    Ok((
        input,
        Annotated {
            tok: Defn::PrimD(name, xs),
            idx: 0,
            len: 0,
        },
    ))
}
