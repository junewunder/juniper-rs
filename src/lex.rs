use crate::annotate::Annotated;
use crate::error::{AnnotatedError, ParseError, ParseErrorKind::{self, *}};
use core::iter::Enumerate;
use core::iter::Map;
use core::slice::Iter;
use nom::bytes::complete::is_a;
use nom::character::complete::alpha1;
use nom::character::complete::newline;
use nom::character::complete::space0;
use nom::combinator::map;
use nom::combinator::not;
use nom::combinator::opt;
use nom::combinator::value;
use nom::error::{
    ErrorKind as NomErrorKind,
    ParseError as NomParseError
};
use nom::number::complete::double;
use nom::Err;
use nom::InputIter;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till, take_until},
    character::complete::space1,
    multi::{fold_many0, many0, many1},
    whitespace, IResult,
};
use std::cell::RefCell;
use std::rc::Rc;

pub type TokenBuffer = Vec<Annotated<Token>>;

struct AnnotationState {
    curr_pos: usize,
}

fn annotated(
    state: Rc<RefCell<AnnotationState>>,
    parser: Box<dyn Fn(&str) -> IResult<&str, Token>>,
) -> impl Fn(&str) -> IResult<&str, Annotated<Token>> {
    move |input: &str| {
        let state = state.clone();
        let prev_pos = state.borrow().curr_pos;
        let prev_len = input.len();
        let (input, result) = parser(input)?;

        let mut state = state.borrow_mut();
        let difference = prev_len - input.len();
        state.curr_pos = prev_pos + difference;

        Ok((
            input,
            Annotated {
                tok: result,
                idx: prev_pos,
                len: difference,
            },
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Delim(String),
    Keywd(String),
    Op(String),
    Prim(String),
    Str(String),
    Space,
    Num(f64),
    Comment,
    Ident(String),
    EOF,
}

pub fn lex(input: &str) -> IResult<&str, TokenBuffer> {
    let state = Rc::new(RefCell::new(AnnotationState { curr_pos: 0 }));

    let tokalt: Box<dyn Fn(&str) -> IResult<&str, Token>> = box |i: &str| {
        alt((
            map(p_comment, |_| Token::Comment),
            p_string,
            p_reserved,
            p_ident,
            map(is_a(" \t\n\r"), |x| Token::Space),
            map(double, |i| Token::Num(i)),
        ))(i)
    };

    let (input, tokbuf) = many0(annotated(state, tokalt))(input)?;

    let mut tokbuf: Vec<_> = tokbuf
        .into_iter()
        .filter(|x| (*x).tok != Token::Comment && (*x).tok != Token::Space)
        .collect();

    tokbuf.push(Annotated {
        tok: Token::EOF,
        idx: tokbuf.last().map_or(0, |x| x.idx + x.len - 1),
        len: 0,
    });

    Ok((input, tokbuf))
}

fn p_comment(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("#")(input)?;
    let (input, _) = take_till(|c| c == '\n' || c == '\0')(input)?;

    Ok((input, Token::Comment))
}

pub fn p_reserved(input: &str) -> IResult<&str, Token> {
    let reserved = (
        map(
            alt((
                tag("if"),
                tag("then"),
                tag("else"),
                tag("while"),
                tag("let"),
                tag("mut"),
                tag("in"),
                tag("fn"),
                tag("prim"),
                tag("struct"),
                tag("enum"),
                tag("match"),
            )),
            |x: &str| Token::Keywd(x.into()),
        ),
        map(alt((tag("true"), tag("false"))), |x: &str| {
            Token::Prim(x.into())
        }),
        map(
            alt((
                tag("=>"),
                tag("&&"),
                tag("||"),
                tag("=="),
                tag("!"),
                tag("<"),
                tag(">"),
                tag("="),
                tag("+"),
                tag("-"),
                tag("*"),
                tag("/"),
                tag("."),
            )),
            |x: &str| Token::Op(x.into()),
        ),
        map(
            alt((
                tag("::"),
                tag("("),
                tag(")"),
                tag(";"),
                tag(":"),
                tag(","),
                tag("{"),
                tag("}"),
            )),
            |x: &str| Token::Delim(x.into()),
        ),
    );

    alt(reserved)(input)
}

#[test]
fn test_string() {
    // println!("{:?}", p_string(r#""hellooo" + x"#));
    println!("{:?}", p_string(r#""Xbeep" + x"#));
}

pub fn p_string(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("\"")(input)?;
    let (input, contents) = opt(escaped_transform(
        // TODO: why is this NOT working
        is_not("\r\n\""),
        '\\',
        |i: &str| {
            println!("INPUT TO TRANSXFORM{:?}", i);
            let r = alt((
                value("florp", tag("beep")),
                value("\\", tag("\\")),
                value("\"", tag("\"")),
                value("n", tag("\n")),
            ))(i);
            println!("RESULT {:?}", r);
            r
        },
    ))(input)?;
    let (input, _) = tag("\"")(input)?;

    Ok((input, Token::Str(contents.unwrap_or(String::new()).into())))
}

pub fn p_ident(input: &str) -> IResult<&str, Token> {
    let (input, _) = not(p_reserved)(input)?;
    let (input, x) = many1(alt((alpha1, is_a("_"))))(input)?;
    Ok((input, Token::Ident(x.join("").to_string())))
}

pub fn take_ident(input: TokenBuffer) -> IResult<TokenBuffer, String, ParseError> {
    let mut t = input.clone();

    if input.len() == 0 {
        return Err(Err::Error((input, TakeIdentError).into()));
    };

    if let Token::Ident(ident) = t.remove(0).tok {
        return Ok((t, ident.clone()));
    };

    Err(Err::Error((input, TakeIdentError).into()))
}

// pub fn match_next<I, O, F>(matches: F) -> impl Fn(Vec<I>) -> IResult<Vec<I>, O, ParseError>
// where
//     F: Fn(I) -> Option<O>,
//     I: Clone + 'static,
// {
//     move |input: Vec<I>| {
//         let mut input = input.clone();
//         let err = |input| {
//             Err(Err::Error(ParseError::from_error_kind(
//                 input,
//                 ErrorKind::TakeTill1,
//             )))
//         };
//
//         if input.len() == 0 {
//             return err(input);
//         };
//
//         let first = input.remove(0);
//         if let Some(out) = matches(first) {
//             return Ok((input, out));
//         };
//
//         err(input)
//     }
// }

/// token tag
pub fn ttag(tag: &Token) -> impl Fn(TokenBuffer) -> IResult<TokenBuffer, Annotated<Token>, ParseError> {
    let tag = tag.clone();
    move |input: TokenBuffer| {
        let tag = tag.clone();
        let mut input = input.clone();
        if input.len() == 0 {
            return Err(Err::Error((input, TokenTagError(tag)).into()));
        };

        let tok = input.remove(0);
        if tag == tok.tok {
            return Ok((input, tok));
        };

        Err(Err::Error((input, TokenTagError(tag)).into()))
    }
}
