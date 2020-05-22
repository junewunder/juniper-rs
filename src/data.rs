use im::hashmap::HashMap;
use std::cell::{Ref, RefCell};
use std::fmt::{self, Display};
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    NumE(f64),
    PlusE(Box<Expr>, Box<Expr>),
    MinusE(Box<Expr>, Box<Expr>),
    MultE(Box<Expr>, Box<Expr>),
    DivE(Box<Expr>, Box<Expr>),
    NegE(Box<Expr>),

    BoolE(bool),
    AndE(Box<Expr>, Box<Expr>),
    OrE(Box<Expr>, Box<Expr>),
    EqE(Box<Expr>, Box<Expr>),
    LtE(Box<Expr>, Box<Expr>),
    GtE(Box<Expr>, Box<Expr>),
    IfE(Box<Expr>, Box<Expr>, Box<Expr>),

    StringE(String),

    VarE(String),
    LetE(String, Box<Expr>, Box<Expr>),

    FnE(String, Box<Expr>),
    AppE(Box<Expr>, Box<Expr>),
    AppPrimE(String, Vec<String>),

    SeqE(Box<Expr>, Box<Expr>),

    WhileE(Box<Expr>, Box<Expr>),
    MutableE(String, Box<Expr>, Box<Expr>),
    MutateE(Box<Expr>, Box<Expr>),
    UnboxE(Box<Expr>),
}
// TODO:
// ClassE(Vec<String>, Vec<(String, Expr)>),
// NewE(Expr, Vec<(String, Expr)>),
// AccessE(Expr, String)

#[derive(Debug)]
pub enum Defn {
    FnD(String, String, Box<Expr>),
    PrimD(String, Vec<String>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    NumV(f64),
    BoolV(bool),
    StringV(String),
    MutV(Rc<RefCell<Box<Value>>>),
    PrimV(String),
    CloV(Option<String>, String, Box<Expr>, Rc<Env>),
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
                let name = name.clone().unwrap_or("anon".to_string());
                write!(f, "<fn {}>", name)
            }
            Null => write!(f, "null"),
        }
    }
}

pub type Env = HashMap<String, Value>;

pub type Store = HashMap<i32, Value>;
