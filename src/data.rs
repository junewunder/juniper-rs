use crate::annotate::Annotated;
use im::hashmap::HashMap;
use std::cell::{Ref, RefCell};
use std::fmt::{self, Display};
use std::rc::Rc;

type Wrap<T> = Box<Annotated<T>>;
type StructFields = Vec<String>;

#[derive(Clone, Debug)]
pub enum Defn {
    FnD(String, String, Wrap<Expr>),
    PrimD(String, Vec<String>),
    StructD(String, Vec<String>),
    EnumD(String, HashMap<String, Vec<String>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    NumE(f64),
    PlusE(Wrap<Expr>, Wrap<Expr>),
    MinusE(Wrap<Expr>, Wrap<Expr>),
    MultE(Wrap<Expr>, Wrap<Expr>),
    DivE(Wrap<Expr>, Wrap<Expr>),
    NegE(Wrap<Expr>),

    BoolE(bool),
    AndE(Wrap<Expr>, Wrap<Expr>),
    OrE(Wrap<Expr>, Wrap<Expr>),
    EqE(Wrap<Expr>, Wrap<Expr>),
    LtE(Wrap<Expr>, Wrap<Expr>),
    GtE(Wrap<Expr>, Wrap<Expr>),
    IfE(Wrap<Expr>, Wrap<Expr>, Wrap<Expr>),

    StringE(String),
    NullE,

    VarE(String),
    LetE(String, Wrap<Expr>, Wrap<Expr>),

    FnE(Option<String>, String, Wrap<Expr>),
    AppE(Wrap<Expr>, Wrap<Expr>),
    AppPrimE(String, Vec<String>),

    SeqE(Wrap<Expr>, Wrap<Expr>),

    NewE(Wrap<Expr>, Vec<(String, Wrap<Expr>)>),
    AccessE(Wrap<Expr>, String),

    WhileE(Wrap<Expr>, Wrap<Expr>),
    MutableE(String, Wrap<Expr>, Wrap<Expr>),
    MutateE(Wrap<Expr>, Wrap<Expr>),
    DerefE(Wrap<Expr>),

    InitStructE(String, HashMap<String, Wrap<Expr>>),
    InitObjectE(Vec<(String, Wrap<Expr>)>),
    InitEnumVariantE(String, String, Vec<String>),

    MatchE(Wrap<Expr>, Vec<(MatchPattern, Wrap<Expr>)>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum MatchPattern {
    VariantPat(String, Vec<MatchPattern>),
    AnyPat(String),
    StringPat(String),
    NumPat(f64),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    NumV(f64),
    BoolV(bool),
    StringV(String),
    MutV(Rc<RefCell<Box<Value>>>),
    RefV(Rc<Value>),
    PrimV(String),
    StructV(String, StructFields),
    ObjectV(Option<String>, HashMap<String, Rc<Value>>),
    CloV(Option<String>, String, Wrap<Expr>, Rc<Env>),
    EnumV(String, String, Vec<Value>),
    NullV,
}

#[derive(Debug)]
pub enum Type {
    NumT,
    BoolT,
    StringT,
    MutT(Box<Type>),
    PrimT,
    CloT,
    NullT,
    AnyT,
    ObjectT,
    StructT(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Value::*;
        match self {
            NumV(n) => write!(f, "{}", n),
            BoolV(b) => write!(f, "{}", b),
            StringV(s) => write!(f, "\"{}\"", s),
            MutV(m) => write!(f, "mut {}", *m.borrow()),
            RefV(m) => write!(f, "ref {}", *m),
            PrimV(p) => write!(f, "<prim {}>", p),
            CloV(name, arg, expr, env) => {
                let name = name.clone().unwrap_or("{anon}".to_string());
                write!(f, "<fn {} :: {} {}>", name, arg, print_env_safe(env))
            }
            ObjectV(name, fields) => {
                let name = name
                    .clone()
                    .map(|name| name + " ")
                    .unwrap_or_else(|| String::new());
                let fields: String = fields
                    .iter()
                    .map(|(k, v)| format!("  {}: {},", k, v))
                    .fold(String::new(), |acc, pair| format!("{}{}\n", acc, pair));
                write!(f, "{}{{\n{}}}", name, fields)
            }
            EnumV(enum_name, variant, args) => {
                let mut args = args.clone();
                let mut args_str = String::new();
                if args.len() > 0 {
                    args_str = format!("{}({}", args_str, args.remove(0));
                    args_str = args
                        .iter()
                        .fold(args_str, |acc, arg| format!("{}, {}", acc, arg));
                    args_str += ")".into();
                }
                write!(f, "{}::{}{}", enum_name, variant, args_str)
            }
            NullV => write!(f, "null"),
            x => write!(f, "{:?}", x),
        }
    }
}

pub fn print_env_safe(env: &Rc<Env>) -> String {
    let mut env_str = format!("{{ ");
    for (k, v) in env.iter() {
        match v {
            Value::CloV(_, arg, _, _) => env_str = format!("{}{}: <fn {}>, ", env_str, k, arg),
            _ => env_str = format!("{}: {}, ", k, v),
        }
    }
    env_str.trim_end_matches(", ");
    format!("{} }}", env_str)
}

pub type Env = HashMap<String, Value>;
pub type TEnv = HashMap<String, Type>;
