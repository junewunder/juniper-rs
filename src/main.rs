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

mod data;
mod interp;
mod lex;
mod mixfix;
mod parse;

use clap::Clap;
use data::*;
use std::fs;

fn main() {
    parse::init_p_expr();

    let env: Env = vec![("print".into(), Value::PrimV("print".into()))]
        .into_iter()
        .collect();

    // let (r, tokbuf) = lex::lex("if true then 1 else 2").expect("expr failed lexing");
    //
    // // println!("remain: {:?}", r);
    // // println!("tokbuf: {:?}", tokbuf);

    println!(
        "{}",
        interp_from_file("./examples/loop.juni").unwrap_or(Value::StringV("null".into()))
    );
    // println!("{:?}", fully_interp_expr("let foo = fn x => y => x + y + 1 in let x = 1 in let y = 2 in foo x y", &env));
    // println!("{:?}", fully_interp_expr("print -1; true", &env));
    // println!("{:?}", fully_interp_expr("let foo = 1 in let bar = 2 in foo", &env));
    // println!("{:?}", fully_interp_expr("fn x => y => print x", &env));
    // println!("{:?}", fully_interp_expr("let foo = fn x => print x in foo 3", &env));
    // println!("{:?}", fully_interp_expr("let foo = fn x => y => print x in foo 3 4", &env));
}

fn interp_from_file(filename: &str) -> Option<Value> {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let input = input.as_ref();
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_defs(tokbuf).expect("expr failed parsing");
    interp::interp_program(ast)
}

fn fully_interp_expr(input: &str, env: &Env) -> Option<Value> {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    interp::interp_expr(ast, env)
}
