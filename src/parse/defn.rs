use crate::annotate::*;
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

pub fn p_defs(input: TokenBuffer) -> IResult<TokenBuffer, Vec<Box<Annotated<Defn>>>> {
    let (input, defs) = many0(annotated_terminal(alt((p_fn_named, p_prim))))(input)?;

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
        Defn::FnD(
            name,
            x_top,
            xs.into_iter().fold(body, |acc, x| box Annotated::zero(
                Expr::FnE(None, x.clone(), acc)
            )),
        )
    ))
}

pub fn p_prim(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_PRIM)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, xs) = separated_nonempty_list(ttag(&T_COMMA), take_ident)(input)?;

    Ok((input, Defn::PrimD(name, xs)))
}

// fn annotated_terminal(
//     parser: Box<dyn Fn(TokenBuffer) -> IResult<TokenBuffer, Defn>>,
// ) -> Rc<Box<dyn Fn(TokenBuffer) -> IResult<TokenBuffer, Box<Annotated<Defn>>>>> {
//     Rc::new(box move |input: TokenBuffer| {
//         let idx = input.first().map(|x| x.idx);
//         let last_idx_len = input.last().map(|x| x.idx + x.len);
//         if idx.is_none() || last_idx_len.is_none() {
//             return Err(Err::Error(ParseError::from_error_kind(
//                 input,
//                 ErrorKind::Tag,
//             )));
//         }
//         let idx = idx.unwrap();
//         let len = last_idx_len.unwrap() - idx;
//
//         let (input, tok) = parser(input)?;
//         let len = input.first().map(|x| x.idx - idx).unwrap_or(len);
//         Ok((input, box Annotated { tok, idx, len }))
//     })
// }
