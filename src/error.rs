use crate::data::{Type, Expr, Value};
use std::error;
use std::fmt::{self, Formatter};
use std::fs;
use std::io::{self, Write};
use std::str;
use nom::error::{
    ErrorKind as NomErrorKind,
    ParseError as NomParseError,
};

pub type ParseError = AnnotatedError<ParseErrorKind>;
pub type TypeError = AnnotatedError<TypeErrorKind>;
pub type InterpError = AnnotatedError<InterpErrorKind>;

#[derive(Debug, Clone)]
pub struct AnnotatedError<ErrorKind> {
    pub kind: ErrorKind,
    pub idx: usize,
    pub len: usize,
    pub loc: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpErrorKind {
    TypeError, // TODO: DynamicTypeError
    StaticTypeError(TypeErrorKind),
    UndefinedError(String),
    DerefError(Value),
    MissingFieldError(String),
    ExtraFieldError(String),
    NoMatchError(Value),
    UnimplementedBehavior,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrorKind {
    TempFiller(i32),
    UndefinedError(String),
    ApplicationError(Type, Type),
    DerefError(Type),
    MissingFieldError(String),
    ExtraFieldError(String),
    UnexpectedTypeError(/*expected*/Type, /*actual*/ Type),
    UnimplementedBehavior,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    NomError(NomErrorKind),
    GenericError
}

impl fmt::Display for TypeErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use InterpErrorKind::*;
        write!(f, "{:?}", self)
    }
}

impl fmt::Display for InterpErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use InterpErrorKind::*;
        match self.clone() {
            TypeError => write!(f, "Type Error"),
            UndefinedError(name) => write!(f, "Undefined variable \"{}\"", name),
            DerefError(value) => write!(f, "Value cannot be dereferenced \"{}\"", value),
            x => write!(f, "{:?}", x),
        }
    }
}

impl fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.clone() {
            ParseErrorKind::NomError(e) => write!(f, "nom error: {:?}", e),
            ParseErrorKind::GenericError => write!(f, "generic error"),
            x => write!(f, "{:?}", x),
        }
    }
}

impl<T: fmt::Display + Clone> fmt::Display for AnnotatedError<T> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use ParseErrorKind::*;

        let err_msg = format!("{}", self.kind.clone());

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

impl error::Error for TypeError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
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

impl From<TypeError> for InterpError {
    fn from(error: TypeError) -> Self {
        AnnotatedError {
            kind: InterpErrorKind::StaticTypeError(error.kind),
            idx: error.idx,
            len: error.len,
            loc: error.loc,
        }
    }
}

fn calc_line<T>(err: &AnnotatedError<T>, contents: &String) -> usize {
    let idx_out_of_bounds = format!("Error position out of bounds for file: {:?}", err.loc);
    let lines: Vec<&str> = contents.split('\n').collect();
    let mut curr_pos = 0;
    let mut line_num = 0;
    while curr_pos < err.idx {
        match lines.get(line_num) {
            Some(line) => {
                curr_pos += line.len();
                line_num += 1;
            }
            None => break
        }
    }
    line_num
}

fn display_from_file(contents: &String, idx: usize, len: usize, line_num: usize) -> String {
    let mut line_num = line_num;
    let contents = contents.as_bytes();
    let contents = &contents[idx..(idx + len)];
    let contents =
        String::from_utf8(contents.to_owned()).expect("Unable to read file to UTF8 format");
    let contents = contents
        .lines()
        .map(|line| {
            line_num += 1;
            format!("{:?}.\t{}\n", line_num - 1, line)
        })
        .collect::<String>()
        .trim_end_matches('\n')
        .to_owned();
    contents
}
