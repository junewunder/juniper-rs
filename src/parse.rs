#![allow(unused)]
use nom::multi::many0;
use crate::data::Defn;
use crate::mixfix::combinator::un_op;
use crate::mixfix::combinator::bin_op;
use nom::multi::many1;
use nom::multi::separated_list;
use nom::multi::separated_nonempty_list;
use crate::lex::ttag;
use crate::lex::take_ident;
use crate::mixfix::mixfix::Mixes;
use std::rc::Rc;
use std::collections::HashMap;
use nom::error::{ParseError, ErrorKind};
use nom::Err;
use core::fmt::Error;
use nom::combinator::map;
use crate::data::{Expr::{self, *}};
use nom::{
    self,
    IResult,
    branch::alt,
    sequence::{delimited},
    bytes::complete::{take_until},
    combinator::{complete, not, peek},
    character::complete::{char, space0, space1, alpha1},
    multi::fold_many0,
    sequence::pair,
};

lazy_static! {
    static ref T_IF: Token = Keywd("if".into());
    static ref T_THN: Token = Keywd("then".into());
    static ref T_ELS: Token = Keywd("else".into());
    static ref T_TRUE: Token = Prim("true".into());
    static ref T_FALSE: Token = Prim("false".into());
    static ref T_LET: Token = Keywd("let".into());
    static ref T_EQ: Token = Op("=".into());
    static ref T_IN: Token = Keywd("in".into());
    static ref T_FN: Token = Keywd("fn".into());
    static ref T_PRIM: Token = Keywd("prim".into());
    static ref T_FAT_ARROW_R: Token = Op("=>".into());
    static ref T_OP_PAREN: Token = Delim("(".into());
    static ref T_CL_PAREN: Token = Delim(")".into());
    static ref T_ADD: Token = Op("+".into());
    static ref T_SUB: Token = Op("-".into());
    static ref T_MULT: Token = Op("*".into());
    static ref T_DIV: Token = Op("/".into());
    static ref T_AND: Token = Op("&&".into());
    static ref T_OR: Token = Op("||".into());
    static ref T_EQEQ: Token = Op("==".into());
    static ref T_LT: Token = Op("<".into());
    static ref T_GT: Token = Op(">".into());
    static ref T_COLONCOLON: Token = Delim("::".into());
    static ref T_COLON: Token = Delim(":".into());
    static ref T_SEMICOLON: Token = Delim(";".into());
    static ref T_COMMA: Token = Delim(",".into());
}

type DefnIResult = IResult<TokenBuffer, Defn>;
type ExprIResult = IResult<TokenBuffer, Box<Expr>>;
type UnOpIResult = IResult<TokenBuffer, Box<dyn UnOp<Box<Expr>>>>;
type BinOpIResult = IResult<TokenBuffer, Box<dyn BinOp<Box<Expr>>>>;
type TokIResult = IResult<TokenBuffer, Token>;

use crate::mixfix::mixfix::*;
use crate::lex::{self, p_ident, p_reserved, TokenBuffer, Token::{self, *}};

const P_SEMI : usize  = 5;
const P_LAM : usize   = 10;
const P_COMMA : usize = 20; // todo
const P_LET : usize   = 30;
const P_ARR : usize   = 40; // todo
const P_OR : usize    = 50;
const P_AND : usize   = 60;
const P_CMP : usize   = 90;
const P_SUM : usize   = 150;
const P_PROD : usize  = 160;
const P_NEG : usize   = 170;
const P_POW : usize   = 180; // todo
const P_FAC : usize   = 190; // todo
const P_APP : usize   = 200; // todo

pub fn make_expr_mixfix() -> MixfixParser<TokenBuffer, Box<Expr>>
{
    let mut levels: HashMap<usize, Rc<Mixes::<TokenBuffer, Box<Expr>>>> = HashMap::new();

    levels.insert(P_SEMI, Rc::new(Mixes {
        infix_l: bin_op(ttag(&T_SEMICOLON), box |lhs, rhs| box SeqE(lhs, rhs)),
        postfix: un_op(ttag(&T_SEMICOLON), box |lhs| lhs),
        ..Mixes::default()
    }));

    levels.insert(P_LAM, Rc::new(Mixes {
        prefix: Rc::new(box p_func),
        ..Mixes::default()
    }));

    levels.insert(P_LET, Rc::new(Mixes {
        prefix: Rc::new(box alt(( p_let, p_if ))),
        ..Mixes::default()
    }));

    Mixes::new_infix_l(&mut levels, P_OR,
        bin_op(ttag(&T_OR), box |lhs, rhs| box OrE(lhs, rhs))
    );

    Mixes::new_infix_l(&mut levels, P_AND,
        bin_op(ttag(&T_AND), box |lhs, rhs| box AndE(lhs, rhs))
    );

    levels.insert(P_CMP, Rc::new(Mixes {
        infix: Rc::new(box alt((p_eq, p_lt, p_gt))),
        ..Mixes::default()
    }));

    levels.insert(P_SUM, Rc::new(Mixes {
        infix_l: Rc::new(box alt((p_add, p_sub))),
        ..Mixes::default()
    }));

    levels.insert(P_PROD, Rc::new(Mixes {
        infix_l: Rc::new(box alt((p_mult, p_div))),
        ..Mixes::default()
    }));

    Mixes::new_prefix(&mut levels, P_NEG,
        un_op(ttag(&T_SUB), box |rhs| box NegE(rhs))
    );

    levels.insert(P_APP, Rc::new(Mixes {
        infix_l: Rc::new(box p_app),
        ..Mixes::default()
    }));

    MixfixParser {
        terminals: Rc::new(box p_terminals),
        levels
    }
}

// TODO: when running multithreaded tests, this is bad. RUST_TEST_THREADS=1
static mut P_EXPR_VALUE: Option<Rc<dyn Parser<TokenBuffer, Box<Expr>>>> = None;

pub fn init_p_expr() {
    unsafe {
        if let Some(_) = P_EXPR_VALUE { return }
        else {
            let mp = make_expr_mixfix();
            P_EXPR_VALUE = Some(mp.build_parser())
        }
    }
}

pub fn p_defs(input: TokenBuffer) -> IResult<TokenBuffer, Vec<Defn>> {
    let (input, defs) = many0(alt((
        p_fn_named,
        p_prim,
        // TODO: top level vars
    )))(input)?;

    Ok((input, defs.into_iter().fold(Vec::new(),
        |mut defs, def| { defs.push(def); defs })))
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

    Ok((input, Defn::FnD(name, x_top, xs.into_iter().fold(
        body,
        |acc, x| box FnE(x.clone(), acc)
    ))))
}

pub fn p_prim(input: TokenBuffer) -> DefnIResult {
    let (input, _) = ttag(&T_PRIM)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, xs) = separated_nonempty_list(
        ttag(&T_COMMA),
        take_ident
    )(input)?;

    Ok((input, Defn::PrimD(name, xs)))
}

pub fn p_expr(input: TokenBuffer) -> ExprIResult {
    unsafe {
        match &P_EXPR_VALUE {
            Some(parser) => parser(input),
            None => panic!(" !!!parser was not initialized!!! ")
        }
    }
}

fn p_terminals(input: TokenBuffer) -> ExprIResult {
    alt((
        p_var,
        p_num,
        p_bool,
        p_parens,
    ))(input)
}

fn p_var(input: TokenBuffer) -> ExprIResult {
    let (input, x) = take_ident(input)?;
    Ok((input, box VarE(x)))
}

fn p_num(input: TokenBuffer) -> ExprIResult {
    let mut input = input.clone();

    if input.len() == 0 {
        let e: ErrorKind = ErrorKind::Float;
        return Err(Err::Error(ParseError::from_error_kind(input, e)))
    };

    if let Num(i) = input.remove(0) {
        return Ok((input, box NumE(i)))
    };

    let e: ErrorKind = ErrorKind::Float;
    Err(Err::Error(ParseError::from_error_kind(input, e)))
}

fn p_bool(input: TokenBuffer) -> ExprIResult {
    let (input, b) = alt((ttag(&T_TRUE), ttag(&T_FALSE)))(input)?;
    let b = b == *T_TRUE;
    Ok((input, box BoolE(b)))
}

fn p_parens(input: TokenBuffer) -> ExprIResult {
    let (input, _) = ttag(&T_OP_PAREN)(input)?;
    let (input, e) = p_expr(input)?;
    let (input, _) = ttag(&T_CL_PAREN)(input)?;
    Ok((input, e))
}

fn p_app(input: TokenBuffer) -> BinOpIResult {
    Ok((input, box move |lhs, rhs|
        box AppE(lhs, rhs)
    ))
}

fn p_if(input: TokenBuffer) -> UnOpIResult {
    let (input, _)    = ttag(&T_IF)(input)?;
    let (input, pred) = p_expr(input)?;
    let (input, _)    = ttag(&T_THN)(input)?;
    let (input, thn)  = p_expr(input)?;
    let (input, _)    = ttag(&T_ELS)(input)?;
    Ok((input, box move |els|
        box IfE(pred.clone(), thn.clone(), els.clone())
    ))
}

pub fn p_let(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_LET)(input)?;
    let (input, x) = take_ident(input)?;
    let (input, _) = ttag(&T_EQ)(input)?;
    let (input, e) = p_expr(input)?;
    let (input, _) = ttag(&T_IN)(input)?;
    Ok((input, box move |body| {
        box LetE(x.clone(), e.clone(), body.clone())}
    ))
}

pub fn p_func(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_FN)(input)?;
    let (input, xs) = many1(|input| {
        let (input, x) = take_ident(input)?;
        let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
        Ok((input, x))
    })(input)?;
    Ok((input, box move |body| {
        xs.iter().rev()
            .fold(body, |acc, x| box FnE(x.clone(), acc))}
    ))
}

pub fn p_seq(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_SEMICOLON)(input)?;
    Ok((input, box move |lhs, rhs|
        box SeqE(lhs, rhs)
    ))
}

pub fn p_add(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_ADD)(input)?;
    Ok((input, box |lhs, rhs|
        box PlusE(lhs, rhs)
    ))
}

pub fn p_sub(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_SUB)(input)?;
    Ok((input, box |lhs, rhs|
        box MinusE(lhs, rhs)
    ))
}

pub fn p_mult(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_MULT)(input)?;
    Ok((input, box |lhs, rhs|
        box MultE(lhs, rhs)
    ))
}

pub fn p_div(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_DIV)(input)?;
    Ok((input, box |lhs, rhs|
        box DivE(lhs, rhs)
    ))
}

pub fn p_eq(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_EQEQ)(input)?;
    Ok((input, box move |lhs, rhs|
        box EqE(lhs, rhs)
    ))
}

pub fn p_lt(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_LT)(input)?;
    Ok((input, box move |lhs, rhs|
        box LtE(lhs, rhs)
    ))
}

pub fn p_gt(input: TokenBuffer) -> BinOpIResult {
    let (input, _) = ttag(&T_GT)(input)?;
    Ok((input, box move |lhs, rhs|
        box GtE(lhs, rhs)
    ))
}

///////////////////////////////////////////
/////// COPY-CAT'ED NOM COMBINATORS ///////
///////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &str) -> TokenBuffer {
        lex::lex(input).unwrap().1
    }

    #[test]
    fn test_expr_var() {
        init_p_expr();
        let res = complete(p_expr)(lex("hello"));
        assert!(res.is_ok());
    }

    #[test]
    fn test_expr_let() {
        init_p_expr();
        let res = complete(p_expr)(lex("let x = 1 in x"));
        assert!(res.is_ok());
    }

    #[test]
    fn test_expr_if() {
        init_p_expr();
        let res = complete(p_expr)(lex("if true then 1 else 2"));
        assert!(res.is_ok());
    }

    #[test]
    fn test_expr_add() {
        init_p_expr();
        // TODO: why does this need parens?
        let res = complete(p_expr)(lex("1 + 2 + 5 + (if true then 1 else 2)"));
        assert!(res.is_ok());
        // println!("{:?}", res);
    }

    #[test]
    fn test_expr_app_1() {
        init_p_expr();
        let res = complete(p_expr)(lex("f 1 (f foo bar) 3 4 true false 7 8"));
        assert!(res.is_ok());
        // println!("{:?}", res);
    }
}
