use std::fmt::{self, Display};

pub(in crate::compiler) enum C1 {
    VarE(String),
    NumE(f64),
    BoolE(bool),
    IfE(Box<C1>, Box<C1>, Box<C1>),
    LetE(String, Box<C1>, Box<C1>),
    AppE(String, Box<C1>),
}

pub struct WatModule {
    defns: Vec<WatDefn>,
    exports: Vec<String>,
}

pub enum WatDefn {
    // TypeWD(Option<String>, Vec<(String, WatType)>, Vec<WatType>),
    // ImportWD,
    Func {
        name: String,
        params: Vec<(String, WatType)>,
        result: WatType,
        locals: Vec<(String, WatType)>,
        body: Vec<WatInstr>,
    }
}

// referenced from https://github.com/rustwasm/walrus/blob/master/src/ir/mod.rs
pub enum WatInstr {
    Block(Vec<WatInstr>),
    Loop(Vec<WatInstr>),
    Call(String),
    Branch(String),
    BranchIf(String),
    IfElse(Vec<WatInstr>, Vec<WatInstr>),

    LocalGet(String),
    LocalSet(String),
    LocalTee(String),
    GlobalGet(String),
    GlobalSet(String),

    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),

    BinOp(String, Box<WatInstr>, Box<WatInstr>),
    UnOp(String, Box<WatInstr>),
}

pub enum WatLit {
    IdentWL(String),
    // NameWL(String), // what is this?
    NumWL(f64),
    StringWL(String),
}

pub enum WatType {
    I32,
    I64,
    F32,
    F64,
    Func(Vec<(String, WatType)>, Vec<WatType>),
    Global(Box<WatType>),
    MutGlobal(Box<WatType>),
}


impl Display for WatModule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::from("(module\n");
        for defn in self.defns.iter() {
            out = out + &indent(format!("{}", defn))
        }
        write!(f, "{}\n)", out)
    }
}

impl Display for WatDefn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = String::from("");

        match self {
            WatDefn::Func{ name, params, result, locals, body } => {
                out += "(function ";
                out += &format!("${} ", name);

                for (n, t) in params.iter() {
                    out += &format!("(param ${} {}) ", n, t);
                }

                out += &format!("(result {}) ", result);

                for (n, t) in locals.iter() {
                    out += &indent(format!("(local ${} {}) ", n, t));
                }

                for instr in body.iter() {
                    out += &indent(format!("{}", instr));
                }
            }
        }

        write!(f, "{}\n)", out)
    }
}

impl Display for WatType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "WAT_TYPE")
    }
}

impl From<crate::data::Type> for WatType {
    fn from(_: crate::data::Type) -> Self {
        WatType::F32
    }
}

impl Display for WatInstr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // WatInstr::Block(is) => {
            //     write!(f, "(block)")
            // }
            // WatInstr::Loop
            WatInstr::Call(x) => {
                write!(f, "(call {})", x)
            }
            // WatInstr::Branch
            // WatInstr::BranchIf
            // WatInstr::IfElse

            WatInstr::LocalGet(x) => write!(f, "(local.get {})", x),
            WatInstr::LocalSet(x) => write!(f, "(local.set {})", x),
            WatInstr::LocalTee(x) => write!(f, "(local.tee {})", x),
            WatInstr::GlobalGet(x) => write!(f, "(global.get {})", x),
            WatInstr::GlobalSet(x) => write!(f, "(global.set {})", x),

            WatInstr::I32Const(n) => write!(f, "i32.const {}", n),
            WatInstr::I64Const(n) => write!(f, "i64.const {}", n),
            WatInstr::F32Const(n) => write!(f, "f32.const {}", n),
            WatInstr::F64Const(n) => write!(f, "f64.const {}", n),

            WatInstr::BinOp(x, lhs, rhs) => {
                let lhs = format!("{}", lhs);
                let rhs = format!("{}", rhs);
                write!(f, "({} \n{}\n{})", x, indent(lhs), indent(rhs))
            },
            WatInstr::UnOp(x, rhs) => {
                let rhs = format!("{}", rhs);
                write!(f, "({} \n{})", x, indent(rhs))
            },

            _ => write!(f, "WAT_INSTR"),
        }
    }
}

pub fn print_instr_vec(instrs: &Vec<WatInstr>) -> String {
    let mut out = String::new();
    for instr in instrs.iter() {
        out += &indent(format!("{}", instr));
        out += "\n";
    }
    out
}

pub fn indent(s: String) -> String {
    format!("    {}", s.replace("\n", "\n    "))
}
