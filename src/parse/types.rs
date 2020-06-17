use crate::annotate::*;
use crate::data::{Defn, Expr, Type};
use crate::lex::{
    self, take_ident, ttag,
    Token::{self, *},
    TokenBuffer,
};
use crate::parse::{expr::p_expr, shared::*};
use crate::mixfix::mixfix::*;
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

#[rustfmt::skip]
fn make_type_mixfix() -> MixfixParser<TokenBuffer, Type> {
    let mut levels: HashMap<usize, Rc<Mixes<TokenBuffer, Type>>> = HashMap::new();

    macro_rules! new_op {
        ($precedence:tt { $($kind:ident : $parser:expr)* }) => {
            levels.insert(
                $precedence,
                Rc::new(Mixes {
                    $($kind: Rc::new(Box::new($parser)),)*
                    ..Mixes::default()
                }),
            );
        }
    }

    new_op!(10 { infix_r: box p_fn });

    new_op!(30 { prefix: box p_mut });

    new_op!(100 { infix_l: box p_app });

    MixfixParser {
        terminals: Rc::new(box p_terminals),
        levels,
    }
}

// TODO: when running multithreaded tests, this is bad. RUST_TEST_THREADS=1
static mut P_TYPE_VALUE: Option<Rc<dyn Parser<TokenBuffer, Type>>> = None;

pub fn init_p_type() {
    unsafe {
        if let Some(_) = P_TYPE_VALUE {
            return;
        } else {
            let mp = make_type_mixfix();
            P_TYPE_VALUE = Some(mp.build_parser())
        }
    }
}

pub fn p_type(input: TokenBuffer) -> TypeIResult {
    unsafe {
        if P_TYPE_VALUE.is_none() {
            init_p_type();
        }

        match &P_TYPE_VALUE {
            Some(parser) => parser(input),
            None => panic!(" !!!parser could not be initialized!!! "),
        }
    }
}

pub fn p_terminals(input: TokenBuffer) -> TypeIResult {
    alt((
        map(ttag(&T_TY_NUM), |_| Type::NumT),
        map(ttag(&T_TY_BOOL), |_| Type::BoolT),
        map(ttag(&T_TY_ANY), |_| Type::AnyT),
        map(ttag(&T_TY_UNIT), |_| Type::UnitT),
        map(ttag(&T_TY_STRING), |_| Type::StringT),
        map(take_ident, |x| Type::TypeVar(x)),
        p_unit,
        p_parens,
    ))(input)
}

fn p_app(input: TokenBuffer) -> IResult<TokenBuffer, Box<dyn BinOp<Type>>> {
    Ok((input, box move |lhs, rhs| Type::ConcreteT(Box::new(lhs), Box::new(rhs))))
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

macro_rules! unop {
    ($name:ident, $tok:ident, $variant:ident) => {
        pub fn $name(input: TokenBuffer) -> IResult<TokenBuffer, Box<dyn UnOp<Type>>> {
            let (input, _) = ttag(&$tok)(input)?;
            Ok((input, box |x| Type::$variant(box x)))
        }
    };
}

macro_rules! binop {
    ($name:ident, $tok:ident, $variant:ident) => {
        pub fn $name(input: TokenBuffer) -> IResult<TokenBuffer, Box<dyn BinOp<Type>>> {
            let (input, _) = ttag(&$tok)(input)?;
            Ok((input, box |lhs, rhs| {
                Type::$variant(box lhs, box rhs)}))
        }
    };
}

unop!(p_mut, T_MUT, MutT);
binop!(p_fn, T_THIN_ARROW_R, CloT);
