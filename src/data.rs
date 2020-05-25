use crate::annotate::Annotated;
use im::hashmap::HashMap;
use std::cell::{Ref, RefCell};
use std::fmt::{self, Display};
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    NumE(f64),
    PlusE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    MinusE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    MultE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    DivE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    NegE(Box<Annotated<Expr>>),

    BoolE(bool),
    AndE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    OrE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    EqE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    LtE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    GtE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    IfE(
        Box<Annotated<Expr>>,
        Box<Annotated<Expr>>,
        Box<Annotated<Expr>>,
    ),

    StringE(String),

    VarE(String),
    LetE(String, Box<Annotated<Expr>>, Box<Annotated<Expr>>),

    FnE(Option<String>, String, Box<Annotated<Expr>>),
    AppE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    AppPrimE(String, Vec<String>),

    SeqE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),

    WhileE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    MutableE(String, Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    MutateE(Box<Annotated<Expr>>, Box<Annotated<Expr>>),
    DerefE(Box<Annotated<Expr>>),
}
// TODO:
// ClassE(Vec<String>, Vec<(String, Expr)>),
// NewE(Expr, Vec<(String, Expr)>),
// AccessE(Expr, String)

#[derive(Debug)]
pub enum Defn {
    FnD(String, String, Box<Annotated<Expr>>),
    PrimD(String, Vec<String>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    NumV(f64),
    BoolV(bool),
    StringV(String),
    MutV(Rc<RefCell<Box<Value>>>),
    PrimV(String),
    CloV(Option<String>, String, Box<Annotated<Expr>>, Rc<Env>),
    Null,
}
// | ClassV [String] [(String, Expr)] Env
// | ObjectV (Map String Integer) (Map String Expr) Env
// deriving (Eq,Ord,Show)

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            NumV(n) => write!(f, "{}", n),
            BoolV(b) => write!(f, "{}", b),
            StringV(s) => write!(f, "{}", s),
            MutV(m) => write!(f, "mut {}", *m.borrow()),
            PrimV(p) => write!(f, "<prim {}>", p),
            CloV(name, arg, expr, env) => {
                let name = name.clone().unwrap_or("{anon}".to_string());
                write!(f, "<fn {} :: {} {}>", name, arg, print_env_safe(env))
            }
            Null => write!(f, "null"),
        }
    }
}

pub fn print_env_safe(env: &Rc<Env>) -> String {
    let mut env_str = format!("{{ ");
    for (k, v) in env.iter() {
        match v {
            Value::CloV(_, arg, _, _) => {
                env_str = format!("{}{}: <fn {}>, ", env_str, k, arg)
            },
            _ => env_str = format!("{}: {}, ", k, v),
        }
    }
    env_str.trim_end_matches(", ");
    format!("{} }}", env_str)
}

pub type Env = HashMap<String, Value>;

pub type Store = HashMap<i32, Value>;
