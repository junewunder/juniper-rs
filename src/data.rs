use std::rc::Rc;
use im::hashmap::HashMap;

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
    VarE(String),
    LetE(String, Box<Expr>, Box<Expr>),
    FnE(String, Box<Expr>),
    AppE(Box<Expr>, Box<Expr>),
    AppPrimE(String, Vec<String>),
    SeqE(Box<Expr>, Box<Expr>)
    // BoxE(Expr),
    // UnboxE(Expr),
    // AssignE(Expr, Expr),
    // ClassE(Vec<String>, Vec<(String, Expr)>),
    // NewE(Expr, Vec<(String, Expr)>),
    // AccessE(Expr, String)
}

#[derive(Debug)]
pub enum Defn {
    FnD(String, String, Box<Expr>),
    PrimD(String, Vec<String>)
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    NumV(f64),
    BoolV(bool),
    CloV(Option<String>, String, Box<Expr>, Rc<Env>),
    PrimV(String),
    Null,
}
  // | LocV Integer
  // | ClassV [String] [(String, Expr)] Env
  // | ObjectV (Map String Integer) (Map String Expr) Env
  // deriving (Eq,Ord,Show)

pub type Env = HashMap<String, Value>;

pub type Store = HashMap<i32, Value>;
