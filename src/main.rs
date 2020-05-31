#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(box_syntax)]
#![feature(get_mut_unchecked)]
#![allow(unused)]

extern crate nom;
#[macro_use]
extern crate im;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate clap;

mod annotate;
mod data;
mod error;
mod interp;
mod lex;
mod mixfix;
mod parse;

use crate::annotate::Annotated;
use crate::data::Defn;
use crate::lex::TokenBuffer;
use crate::parse::ExprIResult;
use crate::error::{ParseError, ParseErrorKind};
use clap::Clap;
use data::*;
use std::fs;

#[derive(Clap, Debug)]
struct Opts {
    /// target file to run
    #[clap(default_value = "./examples/fibonacci.juni")]
    target: String,

    #[clap(long, short("p"))]
    parse: bool,

    #[clap(long, short("l"))]
    lex: bool,

    #[clap(long("no-run"))]
    no_run: bool,
}

fn main() {
    let opts: Opts = Opts::parse();

    if opts.lex {
        let input = fs::read_to_string(opts.target.as_str()).expect("Unable to read file");
        lex::lex(input.clone().as_str())
            .map(|v| println!("{:#?}", v))
            .map_err(|mut e| println!("{:#?}", e));
    }

    if opts.parse {
        parse_from_file(opts.target.as_str())
            .map(|v| println!("{:#?}", v))
            .map_err(|mut e| println!("{:#?}", e));
    }

    if !opts.no_run {
        interp_from_file(opts.target.as_str())
            .map(|v| println!("{}", v))
            .map_err(|mut e| {
                e.loc = Some(opts.target);
                println!("{}", e)
            });
    }
}

fn interp_from_file(filename: &str) -> interp::InterpResult {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let input = input.as_ref();
    let filename = String::from(filename);
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_defs(tokbuf).expect("expr failed parsing");
    interp::interp_program(ast)
}

use nom::IResult;
fn parse_from_file(filename: &str) -> IResult<TokenBuffer, Vec<Box<Annotated<Defn>>>, ParseError> {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let input = input.as_ref();
    let filename = String::from(filename);
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    parse::p_defs(tokbuf)
}

fn interp_expr(input: &str, env: &Env) -> interp::InterpResult {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    interp::interp_expr(ast, env)
}

fn parse_expr(input: &str) -> Box<annotate::Annotated<Expr>> {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    ast
}

fn lex_expr(input: &str) -> IResult<&str, TokenBuffer> {
    lex::lex(input)
}

// Some random expressions for manual testing
// println!("{:?}", fully_interp_expr("let foo = fn x => y => x + y + 1 in let x = 1 in let y = 2 in foo x y", &env));
// println!("{:?}", fully_interp_expr("print -1; true", &env));
// println!("{:?}", fully_interp_expr("let foo = 1 in let bar = 2 in foo", &env));
// println!("{:?}", fully_interp_expr("fn a :: x => y => print x", &env));
// println!("{:?}", fully_interp_expr("let foo = fn x => print x in foo 3", &env));
// println!("{:?}", fully_interp_expr("let foo = fn x => y => print x in foo 3 4", &env));

// println!("{:?}", fully_parse_expr("x => y => x + y + 1"));
// println!("{:?}", fully_parse_expr("let foo = fn x => y => x + y + 1 in let x = 1 in let y = 2 in foo x y"));
// println!("{:#?}", fully_parse_expr("print -1; true"));
// println!(
//     "{:#?}",
//     fully_parse_expr("let foo = 1 in let bar = 2 in foo")
// );
// println!("{:#?}", fully_parse_expr("fn x => y => print x"));
// println!(
//     "{:#?}",
//     fully_parse_expr("let foo = fn x => print x in foo 3")
// );
// println!(
//     "{:#?}",
//     fully_parse_expr("let foo = fn x => fn y => print x in foo 3 4")
// );
