use core::slice::Iter;
use core::iter::Enumerate;
use core::iter::Map;
use nom::InputIter;
use nom::bytes::complete::is_a;
use nom::character::complete::newline;
use nom::combinator::map;
use nom::number::complete::double;
use nom::character::complete::alpha1;
use nom::combinator::not;
use nom::character::complete::space0;
use nom::error::{ParseError, ErrorKind};
use nom::Err;
use nom::{
    IResult,
    branch::{alt},
    bytes::complete::{tag, take_till},
    character::complete::{space1},
    multi::{many0, fold_many0},
    whitespace
};

pub type TokenBuffer = Vec<Token>;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Delim(String),
    Keywd(String),
    Op(String),
    Prim(String),
    Space,
    Num(f64),
    Comment,
    Ident(String)
}

pub fn lex(input: &str) -> IResult<&str, TokenBuffer> {
    let (input, tokbuf) = fold_many0(
        alt((
            map(p_comment, |_| Token::Comment),
            p_reserved,
            p_ident,
            map(is_a(" \t\n\r"), |x| Token::Space),
            map(double, |i| Token::Num(i))
        )),
        Vec::new(),
        |mut acc: Vec<_>, tok| {
            acc.push(tok);
            acc
        }
    )(input)?;

    let tokbuf = tokbuf
        .into_iter()
        .filter(|x| *x != Token::Comment && *x != Token::Space)
        .collect();

    Ok((input, tokbuf))
}

fn p_comment(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("#")(input)?;
    let (input, _) = take_till(|c| c == '\n' || c == '\0' )(input)?;

    Ok((input, Token::Comment))
}

pub fn p_reserved(input: &str) -> IResult<&str, Token> {
    let reserved = (
        map(alt((
            tag("if"), tag("then"), tag("else"), tag("let"), tag("in"),
            tag("fn"), tag("prim")
        )), |x: &str| Token::Keywd(x.into())),

        map(alt((
            tag("true"), tag("false")
        )), |x: &str| Token::Prim(x.into())),

        map(alt((
            tag("=>"), tag("&&"), tag("||"), tag("=="),
            tag("<"), tag(">"),
            tag("="), tag("+"), tag("-"), tag("*"), tag("/"),
        )), |x: &str| Token::Op(x.into())),

        map(alt((
            tag("::"),
            tag("("), tag(")"), tag(";"), tag(":"), tag(","),
        )), |x: &str| Token::Delim(x.into()))
    );

    alt(reserved)(input)
}

pub fn p_ident(input: &str) -> IResult<&str, Token> {
    let (input, _) = not(p_reserved)(input)?;
    let (input, x) = alpha1(input)?;
    Ok((input, Token::Ident(x.to_string())))
}

pub fn take_ident(input: TokenBuffer) -> IResult<TokenBuffer, String> {
    let mut t = input.clone();

    if input.len() == 0 {
        let e: ErrorKind = ErrorKind::TakeTill1;
        return Err(Err::Error(ParseError::from_error_kind(input, e)))
    };

    if let Token::Ident(ident) = t.remove(0) {
        return Ok((t, ident.clone()))
    };

    let e: ErrorKind = ErrorKind::TakeTill1;
    Err(Err::Error(ParseError::from_error_kind(input, e)))
}

/// token tag
pub fn ttag(tag: &Token) -> impl Fn(TokenBuffer) -> IResult<TokenBuffer, Token> {
    let tag = tag.clone();
    move |input: TokenBuffer| {
        let mut input = input.clone();
        if input.len() == 0 {
            let e: ErrorKind = ErrorKind::Tag;
            return Err(Err::Error(ParseError::from_error_kind(input, e)))
        };

        if input.remove(0) == tag {
            return Ok((input, tag.clone()))
        };

        let e: ErrorKind = ErrorKind::Tag;
        Err(Err::Error(ParseError::from_error_kind(input, e)))
    }
}
