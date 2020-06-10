
type Ident = String;

pub(in crate::compiler) enum C1 {
    VarE(Ident),
    NumE(f64),
    BoolE(bool),
    IfE(Box<C1>, Box<C1>, Box<C1>),
    LetE(Ident, Box<C1>, Box<C1>),
    App(Ident, Vec<C1>)
}
