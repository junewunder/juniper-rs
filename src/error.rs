use crate::data::Value;
use std::error;
use std::fmt::{self, Formatter};
use std::fs;
use std::io::{self, Write};
use std::str;

#[derive(Debug, Clone)]
pub struct InterpError {
    pub kind: InterpErrorKind,
    pub idx: usize,
    pub len: usize,
    pub loc: Option<String>,
}

#[derive(Debug, Clone)]
pub enum InterpErrorKind {
    TypeError,
    UndefinedError(String),
    DerefError(Value),
}

impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use InterpErrorKind::*;

        let err_msg: String = match self.kind.clone() {
            TypeError => format!("Type Error"),
            UndefinedError(name) => format!("Undefined variable \"{}\"", name),
            DerefError(value) => format!("Value cannot be dereferenced \"{}\"", value),
            x => format!("some error {:?}", x),
        };

        match self.loc.clone() {
            Some(loc) => {
                let contents = fs::read_to_string(loc.as_str())
                    .expect(&format!("Unable to read file \"{}\"", loc));
                let snippet = display_from_file(&contents, self.idx, self.len);
                write!(
                    f,
                    "{} at line {}\n\t{}",
                    err_msg,
                    calc_line(&self, &contents),
                    snippet
                )
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

fn calc_line(err: &InterpError, contents: &String) -> usize {
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

fn display_from_file(contents: &String, idx: usize, len: usize) -> String {
    let contents = contents.as_bytes();
    let contents = &contents[idx..(idx + len)];
    String::from_utf8(contents.to_owned()).expect("Unable to read file to UTF8 format")
}
