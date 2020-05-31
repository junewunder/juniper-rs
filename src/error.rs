use crate::data::Value;
use crate::lex::{Token, TokenBuffer};
use std::error;
use std::fmt::{self, Formatter};
use std::fs;
use std::io::{self, Write};
use std::str;
use nom::error::{
    ErrorKind as NomErrorKind,
    ParseError as NomParseError,
};

pub type InterpError = AnnotatedError<InterpErrorKind>;

#[derive(Debug, Clone)]
pub struct AnnotatedError<ErrorKind> {
    pub kind: ErrorKind,
    pub idx: usize,
    pub len: usize,
    pub loc: Option<String>,
}

#[derive(Debug, Clone)]
pub enum InterpErrorKind {
    TypeError,
    UndefinedError(String),
    DerefError(Value),
    MissingFieldError(String),
    ExtraFieldError(String),
    NoMatchError(Value),
    UnimplementedBehavior,
}

impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use InterpErrorKind::*;

        let err_msg: String = match self.kind.clone() {
            TypeError => format!("Type Error"),
            UndefinedError(name) => format!("Undefined variable \"{}\"", name),
            DerefError(value) => format!("Value cannot be dereferenced \"{}\"", value),
            x => format!("{:?}", x),
        };

        match self.loc.clone() {
            Some(loc) => {
                let contents = fs::read_to_string(loc.as_str())
                    .expect(&format!("Unable to read file \"{}\"", loc));
                let line = calc_line(&self, &contents);
                let snippet = display_from_file(&contents, self.idx, self.len, line);
                write!(f, "{} at line {}\n{}", err_msg, line, snippet)
            }
            None => write!(f, "{} in <unknown file>", err_msg),
        }
    }
}

impl error::Error for InterpError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

//////////////////////////////////////////////////////////////
///////////////////////////PARSE ERRORS///////////////////////
//////////////////////////////////////////////////////////////


// TODO: parameterize AnnotatedError, move to annotate
pub type ParseError = AnnotatedError<ParseErrorKind>;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    NomError(NomErrorKind),
    Trace(Box<ParseError>, Box<ParseError>),
    TokenTagError(Token),
    TakeIdentError,
    TakeNumberError,
    TakeStringError,
    GenericError
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use ParseErrorKind::*;

        let err_msg: String = match self.kind.clone() {
            ParseErrorKind::NomError(e) => format!("nom error: {:?}", e),
            ParseErrorKind::GenericError => format!("generic error"),
            x => format!("{:?}", x),
        };

        match self.loc.clone() {
            Some(loc) => {
                let contents = fs::read_to_string(loc.as_str())
                    .expect(&format!("Unable to read file \"{}\"", loc));
                let line = calc_line(&self, &contents);
                let snippet = display_from_file(&contents, self.idx, self.len, line);
                write!(f, "{} at line {}\n{}", err_msg, line, snippet)
            }
            None => write!(f, "{} in <unknown file>", err_msg),
        }
    }
}

impl nom::error::ParseError<TokenBuffer> for ParseError {
    fn from_error_kind(input: TokenBuffer, kind: NomErrorKind) -> Self {
        AnnotatedError {
            kind: ParseErrorKind::NomError(kind),
            idx: input.first().map_or(0, |x| x.idx),
            len: input.first().map_or(0, |x| x.len),
            loc: None,
        }
    }

    fn append(input: TokenBuffer, kind: NomErrorKind, other: Self) -> Self {
        ParseError::from_error_kind(input, kind)
        // use ParseErrorKind::*;
        // let
        // AnnotatedError {
        //     kind: Trace(NomError(kind), other),
        //     idx: input.first().map_or(0, |x| x.idx),
        //     len: input.first().map_or(0, |x| x.len),
        //     loc: None,
        // }
    }
}

impl error::Error for ParseError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

impl From<(crate::lex::TokenBuffer, NomErrorKind)> for ParseError {
    fn from(error: (crate::lex::TokenBuffer, NomErrorKind)) -> Self {
        AnnotatedError {
            kind: ParseErrorKind::NomError(error.1),
            idx: error.0.first().map_or(0, |x| x.idx),
            len: error.0.last().map_or(0, |x| x.len),
            loc: None,
        }
    }
}

impl From<(crate::lex::TokenBuffer, ParseErrorKind)> for ParseError {
    fn from(error: (crate::lex::TokenBuffer, ParseErrorKind)) -> Self {
        AnnotatedError {
            kind: error.1,
            idx: error.0.first().map_or(0, |x| x.idx),
            len: error.0.last().map_or(0, |x| x.len),
            loc: None,
        }
    }
}

fn calc_line<T>(err: &AnnotatedError<T>, contents: &String) -> usize {
    let idx_out_of_bounds = format!("Error position out of bounds for file: {:?}", err.loc);
    let lines: Vec<&str> = contents.split('\n').collect();
    let mut curr_pos = 0;
    let mut line_num = 0;
    while curr_pos < err.idx {
        line_num += 1;
        curr_pos += lines
            .get(line_num - 1)
            .expect(idx_out_of_bounds.as_ref())
            .len();
    }
    line_num
}

fn display_from_file(contents: &String, idx: usize, len: usize, line_num: usize) -> String {
    let mut line_num = line_num - 1;
    let contents = contents.as_bytes();
    let contents = &contents[idx..(idx + len)];
    let contents =
        String::from_utf8(contents.to_owned()).expect("Unable to read file to UTF8 format");
    let contents = contents
        .lines()
        .map(|line| {
            line_num += 1;
            format!("{:?}.\t{}\n", line_num, line)
        })
        .collect::<String>()
        .trim_end_matches('\n')
        .to_owned();
    // TODO: This doesn't fix display from file problem for some reason
    // let start = idx.min(contents.len() - 1);
    // let end = (idx + len).min(contents.len() - 1);
    // let contents = &contents[start..end];
    contents
}
