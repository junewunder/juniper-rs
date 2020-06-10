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
    TypeError,
    UndefinedError(String),
    DerefError(Value),
    MissingFieldError(String),
    ExtraFieldError(String),
    NoMatchError(Value),
    UnimplementedBehavior,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeErrorKind {
    TempFiller,
    UndefinedError(String),
    DerefError(Type),
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

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    NomError(NomErrorKind),
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

impl error::Error for ParseError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

// impl Into<ParseError> for (crate::lex::TokenBuffer, NomErrorKind) {
//     fn into(self) -> ParseError {
//         AnnotatedError {
//             kind: ParseErrorKind::NomError(self.1),
//             idx: self.0.first().map_or(0, |x| x.idx),
//             len: self.0.last().map_or(0, |x| x.len),
//             loc: None,
//         }
//     }
// }

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
