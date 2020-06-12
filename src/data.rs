use crate::annotate::Annotated;
use im::hashmap::HashMap;
use std::cell::{Ref, RefCell};
use std::fmt::{self, Display};
use std::rc::Rc;

pub type Env = HashMap<String, Value>;
pub type TEnv = HashMap<String, Type>;
type Wrap<T> = Box<Annotated<T>>;
type StructFields = HashMap<String, Type>;

#[derive(Clone, Debug)]
pub enum Defn {
    VarD(String, Vec<String>, Type, Wrap<Expr>),
    PrimD(String, Vec<String>),
    StructD(String, StructFields),
    EnumD(String, HashMap<String, Vec<Type>>),
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
    UnitE,

    VarE(String),
    LetE(String, Wrap<Expr>, Wrap<Expr>),

    FnE(Option<String>, String, Type, Wrap<Expr>),
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
    ObjectV(Option<String>, HashMap<String, Value>),
    CloV(Option<String>, String, Wrap<Expr>, Rc<Env>),
    EnumV(String, String, Vec<Value>),
    UnitV,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    TypeVar(String),
    NumT,
    BoolT,
    StringT,
    AnyT, // TODO: Remove after type parameterization
    UnitT,
    UnknownT,
    MutT(Box<Type>),
    RefT(Box<Type>),
    PrimT,
    CloT(Box<Type>, Box<Type>),
    ObjectT(HashMap<String, Type>),
    StructT(String, StructFields),
    EnumT(String, HashMap<String, Vec<Type>>),
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
                let arg = if arg == "_" { "()" } else { arg };
                let name = name.clone().unwrap_or("{anon}".to_string());
                write!(f, "<fn {} :: {} {}>", name, arg, print_env(env))
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
            UnitV => write!(f, "unit"),
            x => write!(f, "{:?}", x),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;
        match self {
            NumT => write!(f, "num"),
            BoolT => write!(f, "bool"),
            StringT => write!(f, "string"),
            UnitT => write!(f, "()"),
            AnyT => write!(f, "any"),
            TypeVar(name) => write!(f, "{}", name),
            MutT(m) => write!(f, "mut {}", m),
            RefT(m) => write!(f, "ref {}", m),
            CloT(i, o) => {
                write!(f, "{} -> {}", i, o)
            }
            x => write!(f, "{:?}", x),
        }
    }
}

impl IntoIterator for Type {
    type Item = Type;
    type IntoIter = TypeIterator;

    fn into_iter(self) -> Self::IntoIter {
        TypeIterator{ cur: Some(self) }
    }
}

impl std::iter::FromIterator<Type> for Type {
    fn from_iter<T>(iter: T) -> Self
        where T: IntoIterator<Item = Type>
    {
        let mut out: Option<Type> = None;

        for next in iter {
            match out {
                Some(cur) => out = Some(Type::CloT(Box::new(cur), Box::new(next))),
                None => out = Some(next)
            }
        }
        out.unwrap_or(Type::UnitT)
    }
}

pub struct TypeIterator {
    cur: Option<Type>
}

impl Iterator for TypeIterator {
    type Item = Type;
    fn next(&mut self) -> Option<Self::Item> {
        match self.cur.clone() {
            Some(Type::CloT(t, rt)) => {
                self.cur = Some(*rt.clone());
                Some(*t)
            }
            Some(t) => {
                self.cur = None;
                Some(t)
            }
            None => None,
        }
    }
}

// impl Type {
//     pub fn to_vec(&self) -> Vec<Type> {
//         match self {
//             Type::CloT(t, rt) => {
//                 let out = Vec::new();
//                 out.push(*t.clone());
//                 out.append(&mut rt.to_vec());
//                 out
//             },
//             _ => Vec::
//         }
//     }
// }

pub fn print_tenv(env: &TEnv) -> String {
    let mut env_str = format!("tenv {{ ");
    for (k, v) in env.iter() {
        env_str += &format!("\n    {}: {}, ", k, v)
    }
    env_str = env_str.trim_end_matches(", ").into();
    env_str + "\n}"
}

pub fn print_env(env: &Rc<Env>) -> String {
    let mut env_str = format!("env {{ ");
    for (k, v) in env.iter() {
        match v {
            Value::CloV(_, arg, _, _) if arg == "_" => {
                env_str += &format!("\n    {}: <fn ()>, ", k)
            }
            Value::CloV(_, arg, _, _) => {
                env_str += &format!("\n    {}: <fn {}>, ", k, arg)
            }
            Value::PrimV(_) => (), // THIS MAKES PRIMS NOT BE PRINTED
            _ => env_str += &format!("\n    {}: {}, ", k, v),
        }
    }
    env_str = env_str.trim_end_matches(", ").into();
    env_str + "\n}"
}
