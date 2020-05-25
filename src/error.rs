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
}

impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use InterpErrorKind::*;

        match self.kind.clone() {
            TypeError => {
                write!(f, "Type Error at line {}\n\t", calc_line(&self))
            },
            UndefinedError(name) => {
                write!(f, "Undefined variable \"{}\" at line {}\n\t", name, calc_line(&self))
            },
            _ => {
                write!(f, "error with expr at line {} (ERROR PRINTING UNIMPLEMENTED)\n\t", calc_line(&self))
            }
        };

        match self.loc.clone() {
            Some(loc) => write!(f, "{}", display_from_file(loc, self.idx, self.len)),
            None => write!(f, "<file location unknown>"),
        };

        write!(f, "")
    }
}

impl error::Error for InterpError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}

// TODO: make this take String file contents and usize idx
fn calc_line(err: &InterpError) -> usize {
    let contents = fs::read_to_string(err.loc.clone().unwrap().as_str()).expect("Unable to read file");
    let idx_out_of_bounds = format!("Error position out of bounds for file: {:?}", err.loc);
    let lines: Vec<&str> = contents.split('\n').collect();
    let mut curr_pos = 0;
    let mut line_num = 0;
    while curr_pos < err.idx {
        line_num += 1;
        curr_pos += lines.get(line_num - 1).expect(idx_out_of_bounds.as_ref()).len();
    }
    line_num
}

fn display_from_file(filename: String, idx: usize, len: usize) -> String {
    let contents = fs::read_to_string(filename.as_str()).expect("Unable to read file");
    let contents = contents.as_bytes();
    let contents = &contents[idx..(idx + len)];
    String::from_utf8(contents.to_owned()).expect("Unable to read file to UTF8 format")
}
