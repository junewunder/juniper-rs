use crate::annotate::*;
use crate::data::Expr::{self, *};
use crate::lex::{
    self, take_ident, ttag,
    Token::{self, *},
    TokenBuffer,
};
use crate::mixfix::{
    combinator::{bin_op, un_op},
    mixfix::*,
};
use crate::parse::types::*;
use core::fmt::Error;
use nom::{
    self,
    branch::alt,
    bytes::complete::take_until,
    character::complete::{alpha1, char, space0, space1},
    combinator::map,
    combinator::{complete, not, opt, peek},
    error::{ErrorKind, ParseError},
    multi::{fold_many0, many0, many1, separated_nonempty_list},
    sequence::delimited,
    sequence::pair,
    Err, IResult,
};
use std::collections::HashMap;
use std::rc::Rc;

const P_SEMI: usize = 5;
const P_LAM: usize = 10;
const P_COMMA: usize = 20; // todo
const P_LET: usize = 30;
const P_ARR: usize = 40; // todo
const P_OR: usize = 50;
const P_AND: usize = 60;
const P_CMP: usize = 90;
const P_SUM: usize = 150;
const P_PROD: usize = 160;
const P_NEG: usize = 170;
const P_POW: usize = 180; // todo
const P_FAC: usize = 190; // todo
const P_APP: usize = 200; // todo
const P_DEREF: usize = 210;

fn a(expr: Expr, tokbuf_before: TokenBuffer, tokbuf_after: TokenBuffer) -> Box<Annotated<Expr>> {
    let idx = tokbuf_before.first().unwrap().idx;
    let len = tokbuf_after.first().map(|x| idx - x.idx).unwrap();

    box Annotated {
        tok: expr,
        idx,
        len,
    }
}

fn make_expr_mixfix() -> MixfixParser<TokenBuffer, Box<Annotated<Expr>>> {
    let mut levels: HashMap<usize, Rc<Mixes<TokenBuffer, Box<Annotated<Expr>>>>> = HashMap::new();

    macro_rules! new_op {
        ($precedence:ident { $($kind:ident : $parser:expr)* }) => {
            levels.insert(
                $precedence,
                Rc::new(Mixes {
                    $($kind: Rc::new(Box::new($parser)),)*
                    ..Mixes::default()
                }),
            );
        }
    }

    new_op!(P_SEMI {
        infix_l: anno_infix_parser(p_seq)
        postfix: |input| {
            let (input, _) = ttag(&T_SEMICOLON)(input)?;
            Ok((input, box |lhs| lhs))
        }
    });

    new_op!(P_LAM {
        prefix: anno_prefix_parser(alt((p_func_named, p_func_anon)))
    });

    new_op!(P_LET {
        prefix: anno_prefix_parser(alt((
            p_let, p_if, p_while, p_mutable, p_mutate,
        )))
    });

    new_op!(P_OR {
        infix_l: anno_infix_parser(p_or)
    });

    new_op!(P_AND {
        infix_l: anno_infix_parser(p_and)
    });

    new_op!(P_CMP {
        infix: anno_infix_parser(alt((p_eq, p_lt, p_gt)))
    });

    new_op!(P_SUM {
        infix_l: anno_infix_parser(alt((p_add, p_sub)))
    });

    new_op!(P_PROD {
        infix_l: anno_infix_parser(alt((p_mult, p_div)))
    });

    new_op!(P_NEG {
        prefix: anno_prefix_parser(p_neg)
    });

    new_op!(P_APP {
        infix_l: anno_infix_parser(p_app)
    });

    new_op!(P_DEREF {
        prefix: anno_prefix_parser(p_deref)
    });

    MixfixParser {
        terminals: annotated_terminal(box p_terminals),
        levels,
    }
}

// TODO: when running multithreaded tests, this is bad. RUST_TEST_THREADS=1
static mut P_EXPR_VALUE: Option<Rc<dyn Parser<TokenBuffer, Box<Annotated<Expr>>>>> = None;

pub fn init_p_expr() {
    unsafe {
        if let Some(_) = P_EXPR_VALUE {
            return;
        } else {
            let mp = make_expr_mixfix();
            P_EXPR_VALUE = Some(mp.build_parser())
        }
    }
}

fn annotated_terminal(
    parser: Box<dyn Fn(TokenBuffer) -> IResult<TokenBuffer, Expr>>,
) -> Rc<Box<dyn Fn(TokenBuffer) -> IResult<TokenBuffer, Box<Annotated<Expr>>>>> {
    Rc::new(box move |input: TokenBuffer| {
        let idx = input.first().map(|x| x.idx);
        let last_idx_len = input.last().map(|x| x.idx + x.len);
        if idx.is_none() || last_idx_len.is_none() {
            return Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        let idx = idx.unwrap();
        let len = last_idx_len.unwrap() - idx;

        let (input, tok) = parser(input)?;
        let len = input.first().map(|x| x.idx - idx).unwrap_or(len);
        Ok((input, box Annotated { tok, idx, len }))
    })
}

pub fn p_expr(input: TokenBuffer) -> IResult<TokenBuffer, Box<Annotated<Expr>>> {
    unsafe {
        if P_EXPR_VALUE.is_none() {
            init_p_expr();
        }

        match &P_EXPR_VALUE {
            Some(parser) => parser(input),
            None => panic!(" !!!parser could not be initialized!!! "),
        }
    }
}

fn p_terminals(input: TokenBuffer) -> ExprIResult {
    alt((p_var, p_num, p_bool, p_string, p_parens))(input)
}

fn p_var(input: TokenBuffer) -> ExprIResult {
    let (input, x) = take_ident(input)?;
    if peek(ttag(&T_FAT_ARROW_R))(input.clone()).is_ok() {
        return Err(Err::Error(ParseError::from_error_kind(input, ErrorKind::Tag)));
    }
    Ok((input, VarE(x)))
}

fn p_bool(input: TokenBuffer) -> ExprIResult {
    let (input, b) = alt((ttag(&T_TRUE), ttag(&T_FALSE)))(input)?;
    let b = b == *T_TRUE;
    Ok((input, BoolE(b)))
}

fn p_parens(input: TokenBuffer) -> ExprIResult {
    let (input, _) = ttag(&T_OP_PAREN)(input)?;
    let (input, e) = p_expr(input)?;
    let (input, _) = ttag(&T_CL_PAREN)(input)?;
    Ok((input, (*e).unwrap()))
}

fn p_app(input: TokenBuffer) -> BinOpIResult {
    Ok((input, box move |lhs, rhs| AppE(lhs, rhs)))
}

fn p_if(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_IF)(input)?;
    let (input, pred) = p_expr(input)?;
    let (input, _) = ttag(&T_THN)(input)?;
    let (input, thn) = p_expr(input)?;
    let (input, _) = ttag(&T_ELS)(input)?;
    Ok((input, box move |els| {
        IfE(pred.clone(), thn.clone(), els.clone())
    }))
}

pub fn p_let(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_LET)(input)?;
    let (input, x) = take_ident(input)?;
    let (input, _) = ttag(&T_EQ)(input)?;
    let (input, e) = p_expr(input)?;
    let (input, _) = ttag(&T_IN)(input)?;
    Ok((input, box move |body| {
        LetE(x.clone(), e.clone(), body.clone())
    }))
}

pub fn p_mutable(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_MUT)(input)?;
    let (input, x) = take_ident(input)?;
    let (input, _) = ttag(&T_EQ)(input)?;
    let (input, e) = p_expr(input)?;
    let (input, _) = ttag(&T_IN)(input)?;
    Ok((input, box move |body| {
        MutableE(x.clone(), e.clone(), body.clone())
    }))
}

pub fn p_mutate(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_BANG)(input)?;
    let (input, x) = p_expr(input)?;
    let (input, _) = ttag(&T_EQ)(input)?;
    Ok((input, box move |e| MutateE(x.clone(), e.clone())))
}

pub fn p_while(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_WHILE)(input)?;
    let (input, pred) = p_expr(input)?;
    let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
    Ok((input, box move |body| WhileE(pred.clone(), body.clone())))
}

pub fn p_func_anon(input: TokenBuffer) -> UnOpIResult {
    let (input, x) = take_ident(input)?;
    let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
    Ok((input, box move |body| FnE(None, x.clone(), body)))
}

pub fn p_func_named(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = opt(ttag(&T_FN))(input)?;
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, arg) = take_ident(input)?;
    let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
    Ok((input, box move |body| FnE(Some(name.clone()), arg.clone(), body)))
}

macro_rules! unop {
    ($name:ident, $tok:ident, $variant:ident) => {
        pub fn $name(input: TokenBuffer) -> UnOpIResult {
            let (input, _) = ttag(&$tok)(input)?;
            Ok((input, box |x| $variant(x)))
        }
    };
}

macro_rules! binop {
    ($name:ident, $tok:ident, $variant:ident) => {
        pub fn $name(input: TokenBuffer) -> BinOpIResult {
            let (input, _) = ttag(&$tok)(input)?;
            Ok((input, box |lhs, rhs| $variant(lhs, rhs)))
        }
    };
}

binop!(p_seq, T_SEMICOLON, SeqE);
binop!(p_add, T_ADD, PlusE);
binop!(p_sub, T_SUB, MinusE);
binop!(p_mult, T_MULT, MultE);
binop!(p_div, T_DIV, DivE);
binop!(p_eq, T_EQEQ, EqE);
binop!(p_lt, T_LT, LtE);
binop!(p_gt, T_GT, GtE);
binop!(p_or, T_OR, OrE);
binop!(p_and, T_AND, AndE);
unop!(p_neg, T_SUB, NegE);
unop!(p_deref, T_MULT, DerefE);

fn p_num(input: TokenBuffer) -> ExprIResult {
    let mut input = input.clone();

    if input.len() == 0 {
        let e: ErrorKind = ErrorKind::Float;
        return Err(Err::Error(ParseError::from_error_kind(input, e)));
    };

    if let Num(i) = input.remove(0).tok {
        return Ok((input, NumE(i)));
    };

    let e: ErrorKind = ErrorKind::Float;
    Err(Err::Error(ParseError::from_error_kind(input, e)))
}

fn p_string(input: TokenBuffer) -> ExprIResult {
    let mut input = input.clone();

    if input.len() == 0 {
        let e: ErrorKind = ErrorKind::Tag;
        return Err(Err::Error(ParseError::from_error_kind(input, e)));
    };

    if let Str(string) = input.remove(0).tok {
        return Ok((input, StringE(string)));
    };

    let e: ErrorKind = ErrorKind::Tag;
    Err(Err::Error(ParseError::from_error_kind(input, e)))
}
