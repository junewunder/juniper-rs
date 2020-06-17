#![feature(type_alias_impl_trait)]
#![feature(trait_alias)]
#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(get_mut_unchecked)]
#![allow(unused)]

extern crate nom;
#[macro_use]
extern crate im;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate clap;
extern crate rand;
extern crate colored;

mod annotate;
mod data;
mod error;
mod interp;
mod lex;
mod mixfix;
mod parse;
mod compiler;
mod typecheck;

use crate::annotate::Annotated;
use crate::data::Defn;
use crate::error::TypeError;
use crate::lex::TokenBuffer;
use crate::parse::ExprIResult;
use colored::*;
use nom::IResult;
use clap::Clap;
use data::*;
use std::fs;
use std::path::PathBuf;

#[derive(Clap, Debug)]
struct Opts {
    /// target file to run
    #[clap(default_value = "./examples/fibonacci.juni")]
    target: String,

    #[clap(long, short("p"))]
    parse: bool,

    #[clap(long, short("l"))]
    lex: bool,

    #[clap(long, short("t"))]
    typecheck: bool,

    #[clap(long("no-run"))]
    no_run: bool,
}

fn main() {
    let opts: Opts = Opts::parse();
    println!("{}", parse_type("num"));
    println!("{}", parse_type("A"));
    println!("{}", parse_type("A -> B"));
    println!("{}", parse_type("A -> B -> C"));
    println!("{}", parse_type("A -> (B -> C) -> D"));
    println!("{}", parse_type("A -> (B -> C) -> (D -> (E -> F) -> G) -> H"));

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

    if opts.typecheck {
        typecheck_from_file(opts.target.clone().as_str())
            .map(|v| println!("{}", print_tenv(&v)))
            .map_err(|mut e| {
                e.loc = Some(opts.target.clone());
                println!("{}", e)
            });
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
    let filename = PathBuf::from(filename);
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, mut ast) = parse::p_defs(tokbuf).expect("expr failed parsing");
    let tenv = typecheck::check_program(filename.clone(), ast.clone())?;
    let env = interp::interp_program(filename, ast)?;

    println!("{}", print_tenv(&tenv));
    println!("{}", print_env(&env));
    if let Some(Value::CloV(_, arg, body, env)) = env.get("main".into()) {
        return interp::interp_expr(body.clone(), &env);
    }

    panic!("no <main> method")
}

fn typecheck_from_file(filename: &str) -> Result<TEnv, TypeError> {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let input = input.as_ref();
    let filename = String::from(filename);
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_defs(tokbuf).expect("expr failed parsing");
    typecheck::check_program(filename.into(), ast)
}

fn parse_from_file(filename: &str) -> IResult<TokenBuffer, Vec<Box<Annotated<Defn>>>> {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let input = input.as_ref();
    let filename = String::from(filename);
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    parse::p_defs(tokbuf)
}

fn interp_expr(input: &str, env: &Env) -> interp::InterpResult {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    let tenv = typecheck::check_expr(ast.clone(), &im::HashMap::new())?;
    interp::interp_expr(ast, env)
}

fn parse_expr(input: &str) -> Box<annotate::Annotated<Expr>> {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    ast
}

fn parse_type(input: &str) -> Type {
    let (r, tokbuf) = lex::lex(input).expect("type failed lexing");
    let (r, ast) = parse::p_type(tokbuf).expect("type failed parsing");
    ast
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
