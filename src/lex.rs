use core::iter::Enumerate;
use core::iter::Map;
use core::slice::Iter;
use nom::bytes::complete::is_a;
use nom::character::complete::alpha1;
use nom::character::complete::newline;
use nom::character::complete::space0;
use nom::combinator::map;
use nom::combinator::not;
use nom::combinator::value;
use nom::error::{ErrorKind, ParseError};
use nom::number::complete::double;
use nom::Err;
use nom::InputIter;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till, take_until},
    character::complete::space1,
    multi::{fold_many0, many0},
    whitespace, IResult,
};

pub type TokenBuffer = Vec<Token>;

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
}

pub fn lex(input: &str) -> IResult<&str, TokenBuffer> {
    let (input, tokbuf) = many0(alt((
        map(p_comment, |_| Token::Comment),
        p_string,
        p_reserved,
        p_ident,
        map(is_a(" \t\n\r"), |x| Token::Space),
        map(double, |i| Token::Num(i)),
    )))(input)?;

    let tokbuf = tokbuf
        .into_iter()
        .filter(|x| *x != Token::Comment && *x != Token::Space)
        .collect();

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
                tag("let"),
                tag("in"),
                tag("fn"),
                tag("prim"),
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
                tag("<"),
                tag(">"),
                tag("="),
                tag("+"),
                tag("-"),
                tag("*"),
                tag("/"),
            )),
            |x: &str| Token::Op(x.into()),
        ),
        map(
            alt((tag("::"), tag("("), tag(")"), tag(";"), tag(":"), tag(","))),
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
    let (input, contents) = escaped_transform( // TODO: why is this NOT working
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
        }
    )(input)?;
    let (input, _) = tag("\"")(input)?;

    Ok((input, Token::Str(contents.into())))
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
        return Err(Err::Error(ParseError::from_error_kind(input, e)));
    };

    if let Token::Ident(ident) = t.remove(0) {
        return Ok((t, ident.clone()));
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
            return Err(Err::Error(ParseError::from_error_kind(input, e)));
        };

        if input.remove(0) == tag {
            return Ok((input, tag.clone()));
        };

        let e: ErrorKind = ErrorKind::Tag;
        Err(Err::Error(ParseError::from_error_kind(input, e)))
    }
}
