use crate::annotate::Annotated;
use crate::data::*;
use crate::lex::{
    take_ident, ttag,
    Token::{self, *},
    TokenBuffer,
};
use crate::mixfix::mixfix::{BinOp, UnOp};
use crate::parse::types::p_type;
use nom::{combinator::opt, multi::separated_list, IResult};

#[rustfmt::skip]
lazy_static! {
    pub static ref T_TRUE: Token           = Prim("true".into());
    pub static ref T_FALSE: Token          = Prim("false".into());
    pub static ref T_TY_NUM: Token         = Prim("num".into());
    pub static ref T_TY_BOOL: Token        = Prim("bool".into());
    pub static ref T_TY_UNIT: Token        = Prim("unit".into());
    pub static ref T_TY_ANY: Token         = Prim("any".into());
    pub static ref T_TY_STRING: Token      = Prim("string".into());
    pub static ref T_IF: Token             = Keywd("if".into());
    pub static ref T_THN: Token            = Keywd("then".into());
    pub static ref T_ELS: Token            = Keywd("else".into());
    pub static ref T_WHILE: Token          = Keywd("while".into());
    pub static ref T_LET: Token            = Keywd("let".into());
    pub static ref T_MUT: Token            = Keywd("mut".into());
    pub static ref T_IN: Token             = Keywd("in".into());
    pub static ref T_FN: Token             = Keywd("fn".into());
    pub static ref T_PRIM: Token           = Keywd("prim".into());
    pub static ref T_STRUCT: Token         = Keywd("struct".into());
    pub static ref T_ENUM: Token           = Keywd("enum".into());
    pub static ref T_MATCH: Token          = Keywd("match".into());
    pub static ref T_IMPORT: Token         = Keywd("import".into());
    pub static ref T_EQ: Token             = Op("=".into());
    pub static ref T_FAT_ARROW_R: Token    = Op("=>".into());
    pub static ref T_THIN_ARROW_R: Token   = Op("->".into());
    pub static ref T_ADD: Token            = Op("+".into());
    pub static ref T_SUB: Token            = Op("-".into());
    pub static ref T_MULT: Token           = Op("*".into());
    pub static ref T_DIV: Token            = Op("/".into());
    pub static ref T_AND: Token            = Op("&&".into());
    pub static ref T_OR: Token             = Op("||".into());
    pub static ref T_EQEQ: Token           = Op("==".into());
    pub static ref T_BANG: Token           = Op("!".into());
    pub static ref T_LT: Token             = Op("<".into());
    pub static ref T_GT: Token             = Op(">".into());
    pub static ref T_DOT: Token            = Op(".".into());
    pub static ref T_COLONCOLON: Token     = Delim("::".into());
    pub static ref T_COLON: Token          = Delim(":".into());
    pub static ref T_SEMICOLON: Token      = Delim(";".into());
    pub static ref T_COMMA: Token          = Delim(",".into());
    pub static ref T_OP_PAREN: Token       = Delim("(".into());
    pub static ref T_CL_PAREN: Token       = Delim(")".into());
    pub static ref T_OP_BRACE: Token       = Delim("{".into());
    pub static ref T_CL_BRACE: Token       = Delim("}".into());
    pub static ref T_OP_BRACKET: Token     = Delim("[".into());
    pub static ref T_CL_BRACKET: Token     = Delim("]".into());
}

pub trait PreAnnoUnOp<O> = Fn(Box<Annotated<O>>) -> O;
pub trait PreAnnoBinOp<O> = Fn(Box<Annotated<O>>, Box<Annotated<O>>) -> O;
pub trait PostAnnoUnOp<O> = UnOp<Box<Annotated<O>>>;
pub trait PostAnnoBinOp<O> = BinOp<Box<Annotated<O>>>;

pub type DefnIResult = IResult<TokenBuffer, Defn>;
pub type ExprIResult = IResult<TokenBuffer, Expr>;
pub type TypeIResult = IResult<TokenBuffer, Type>;
pub type UnOpIResult = IResult<TokenBuffer, Box<dyn PreAnnoUnOp<Expr>>>;
pub type BinOpIResult = IResult<TokenBuffer, Box<dyn PreAnnoBinOp<Expr>>>;
pub type TokIResult = IResult<TokenBuffer, Token>;

pub fn p_unit_arg(input: TokenBuffer) -> ExprIResult {
    let (input, _) = ttag(&T_OP_PAREN)(input)?;
    let (input, _) = ttag(&T_CL_PAREN)(input)?;
    Ok((input, Expr::UnitE))
}

pub fn p_var_type_pair(input: TokenBuffer) -> IResult<TokenBuffer, (String, Type)> {
    let (input, x) = take_ident(input)?;
    let (input, _) = ttag(&T_COLON)(input)?;
    let (input, t) = p_type(input)?;
    Ok((input, (x, t)))
}
