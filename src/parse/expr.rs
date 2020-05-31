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
use crate::parse::shared::*;
use crate::error::{AnnotatedError, ParseError, ParseErrorKind};
use core::fmt::Error;
use nom::{
    self,
    branch::alt,
    bytes::complete::take_until,
    character::complete::{alpha1, char, space0, space1},
    combinator::map,
    combinator::{complete, not, opt, peek},
    error::{ErrorKind as NomErrorKind, ParseError as NomParseError},
    multi::{fold_many0, many0, many1, separated_list, separated_nonempty_list},
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
const P_DEREF: usize = 35;
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
const P_ACCESS: usize = 210;

#[rustfmt::skip]
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
            Ok((input, box |lhs| box Annotated::zero(SeqE(lhs, box Annotated::zero(NullE)))))
        }
    });

    new_op!(P_LAM { prefix: anno_prefix_parser(alt((p_func_named, p_func_anon))) });

    new_op!(P_LET { prefix: anno_prefix_parser(alt((p_let, p_if, p_while, p_mutable, p_mutate))) });

    new_op!(P_DEREF { prefix: anno_prefix_parser(p_deref) });

    new_op!(P_OR { infix_l: anno_infix_parser(p_or) });

    new_op!(P_AND { infix_l: anno_infix_parser(p_and) });

    new_op!(P_CMP { infix: anno_infix_parser(alt((p_eq, p_lt, p_gt))) });

    new_op!(P_SUM { infix_l: anno_infix_parser(alt((p_add, p_sub))) });

    new_op!(P_PROD { infix_l: anno_infix_parser(alt((p_mult, p_div))) });

    new_op!(P_NEG { prefix: anno_prefix_parser(p_neg) });

    new_op!(P_APP { infix_l: anno_infix_parser(p_app) });

    new_op!(P_ACCESS { postfix: anno_postfix_parser(p_access) });

    MixfixParser {
        terminals: Rc::new(box annotated_terminal(box p_terminals)),
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
    alt((
        p_num,
        p_bool,
        p_string,
        p_parens,
        p_init_object,
        p_init_struct,
        p_match,
        p_var,
    ))(input)
}

fn p_var(input: TokenBuffer) -> ExprIResult {
    let (input, x) = take_ident(input)?;
    if peek(ttag(&T_FAT_ARROW_R))(input.clone()).is_ok() {
        return Err(Err::Error(NomParseError::from_error_kind(
            input,
            NomErrorKind::Tag,
        )));
    }
    Ok((input, VarE(x)))
}

fn p_bool(input: TokenBuffer) -> ExprIResult {
    let (input, b) = alt((ttag(&T_TRUE), ttag(&T_FALSE)))(input)?;
    let b = b.unwrap() == *T_TRUE;
    Ok((input, BoolE(b)))
}

fn p_parens(input: TokenBuffer) -> ExprIResult {
    let (input, _) = ttag(&T_OP_PAREN)(input)?;
    let (input, e) = p_expr(input)?;
    let (input, _) = ttag(&T_CL_PAREN)(input)?;
    Ok((input, (*e).unwrap()))
}

fn p_string_expr_pair(input: TokenBuffer) -> IResult<TokenBuffer, (String, Box<Annotated<Expr>>)> {
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_COLON)(input)?;
    let (input, value) = p_expr(input)?;

    Ok((input, (name, value)))
}

fn p_init_struct(input: TokenBuffer) -> ExprIResult {
    let (input, name) = take_ident(input)?;
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, fields) = separated_list(ttag(&T_COMMA), p_string_expr_pair)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;
    Ok((input, InitStructE(name, fields.into())))
}

fn p_init_object(input: TokenBuffer) -> ExprIResult {
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, fields) = separated_list(ttag(&T_COMMA), p_string_expr_pair)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;

    Ok((input, InitObjectE(fields)))
}

use crate::data::MatchPattern;
pub fn p_match(input: TokenBuffer) -> ExprIResult {
    let (input, _) = ttag(&T_MATCH)(input)?;
    let (input, subject) = p_expr(input)?;
    let (input, _) = ttag(&T_OP_BRACE)(input)?;
    let (input, cases) = separated_nonempty_list(ttag(&T_COMMA), p_match_case)(input)?;
    let (input, _) = opt(ttag(&T_COMMA))(input)?;
    let (input, _) = ttag(&T_CL_BRACE)(input)?;
    Ok((input, MatchE(subject, cases)))
}

// TODO: nested patterns
fn p_match_case(input: TokenBuffer) -> IResult<TokenBuffer, (MatchPattern, Box<Annotated<Expr>>)> {
    let (input, pattern) = p_match_pattern(input)?;
    let (input, _) = ttag(&T_FAT_ARROW_R)(input)?;
    let (input, body) = p_expr(input)?;

    Ok((input, (pattern, body)))
}

fn p_match_pattern(input: TokenBuffer) -> IResult<TokenBuffer, MatchPattern> {
    use MatchPattern::*;
    alt((
        map(p_match_enum_field, |(variant, args)| {
            VariantPat(variant, args)
        }),
        map(take_ident, |x| AnyPat(x)),
        map(p_string, |x| {
            if let Expr::StringE(s) = x {
                StringPat(s)
            } else {
                panic!()
            }
        }),
        map(p_num, |x| {
            if let Expr::NumE(n) = x {
                NumPat(n)
            } else {
                panic!()
            }
        }),
    ))(input)
}

fn p_match_enum_field(input: TokenBuffer) -> IResult<TokenBuffer, (String, Vec<MatchPattern>)> {
    let (input, _) = ttag(&T_COLONCOLON)(input)?;
    let (input, name) = take_ident(input)?;
    let (input, variants) = opt(|input| {
        let (input, _) = ttag(&T_OP_PAREN)(input)?;
        let (input, variants) = separated_list(ttag(&T_COMMA), p_match_pattern)(input)?;
        let (input, _) = opt(ttag(&T_COMMA))(input)?;
        let (input, _) = ttag(&T_CL_PAREN)(input)?;
        Ok((input, variants))
    })(input)?;

    Ok((input, (name, variants.unwrap_or(Vec::with_capacity(0)))))
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
    Ok((input, box move |body| {
        FnE(Some(name.clone()), arg.clone(), body)
    }))
}

fn p_access(input: TokenBuffer) -> UnOpIResult {
    let (input, _) = ttag(&T_DOT)(input)?;
    let (input, x) = take_ident(input)?;
    Ok((input, box move |lhs| AccessE(lhs, x.clone())))
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
        let e: NomErrorKind = NomErrorKind::Float;
        return Err(Err::Error(NomParseError::from_error_kind(input, e)));
    };

    if let Num(i) = input.remove(0).tok {
        return Ok((input, NumE(i)));
    };

    let e: NomErrorKind = NomErrorKind::Float;
    Err(Err::Error(NomParseError::from_error_kind(input, e)))
}

fn p_string(input: TokenBuffer) -> ExprIResult {
    let mut input = input.clone();

    if input.len() == 0 {
        let e: NomErrorKind = NomErrorKind::Tag;
        return Err(Err::Error(NomParseError::from_error_kind(input, e)));
    };

    if let Str(string) = input.remove(0).tok {
        return Ok((input, StringE(string)));
    };

    // return Err(nom_err(input, ))
    let e: NomErrorKind = NomErrorKind::Tag;
    Err(Err::Error(NomParseError::from_error_kind(input, e)))
}

fn nom_err(input: TokenBuffer, kind: NomErrorKind) -> nom::Err<ParseError> {
    Err::Error(AnnotatedError {
        kind: ParseErrorKind::NomError(kind),
        idx: input.first().map_or(0, |x| x.idx),
        len: input.last().map_or(0, |x| x.len),
        loc: None,
    })
}
