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
mod annotate;
mod lex;
mod mixfix;
mod parse;
mod error;

use clap::Clap;
use data::*;
use std::fs;

fn main() {
    parse::init_p_expr();

    let filename = "./examples/invalid/02-type-error.juni";
    interp_from_file(filename)
        .map(|v| println!("{}", v))
        .map_err(|mut e| {
            e.loc = Some(filename.into());
            println!("{}", e)
        });

    // println!("{:?}", fully_interp_expr("let foo = fn x => y => x + y + 1 in let x = 1 in let y = 2 in foo x y", &env));
    // println!("{:?}", fully_interp_expr("print -1; true", &env));
    // println!("{:?}", fully_interp_expr("let foo = 1 in let bar = 2 in foo", &env));
    // println!("{:?}", fully_interp_expr("fn x => y => print x", &env));
    // println!("{:?}", fully_interp_expr("let foo = fn x => print x in foo 3", &env));
    // println!("{:?}", fully_interp_expr("let foo = fn x => y => print x in foo 3 4", &env));

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
}

fn interp_from_file(filename: &str) -> interp::InterpResult {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    let input = input.as_ref();
    let filename = String::from(filename);
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_defs(tokbuf).expect("expr failed parsing");
    interp::interp_program(ast)
}

fn fully_interp_expr(input: &str, env: &Env) -> interp::InterpResult {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    interp::interp_expr(ast, env)
}

fn fully_parse_expr(input: &str) -> Box<annotate::Annotated<Expr>> {
    let (r, tokbuf) = lex::lex(input).expect("expr failed lexing");
    let (r, ast) = parse::p_expr(tokbuf).expect("expr failed parsing");
    ast
}
