use std::error;
use std::fmt;
use std::fs;
use std::io::{self, Write};

fn display_from_file(filename: String, idx: usize, len: usize) {
    let contents = fs::read_to_string(filename.as_str()).expect("Unable to read file");
    let contents = contents.as_bytes();
    // let newlines = String::from(&contents[0..idx])
    let contents = &contents[idx..(idx + len)];
    io::stdout().write_all(contents);
}

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
    UndefinedError,
}

impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use InterpErrorKind::*;
        match self.kind {
            TypeError => {
                write!(f, "Type Error @\n\t")
            },
            UndefinedError => {
                write!(f, "Variable undefined: @\n\t")
            },
            _ => {
                write!(f, "error with expr @ ?..? (PRINT UNIMPLEMENTED)")
            }
        };
        self.loc.clone()
            .map(|loc| display_from_file(loc, self.idx, self.len))
            .or_else(|| {
                write!(f, "<file location unknown>");
                None
            });
        write!(f, "")
    }
}

impl error::Error for InterpError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // Generic error, underlying cause isn't tracked.
        None
    }
}
