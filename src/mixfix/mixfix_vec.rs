#![allow(unused)]


use core::fmt::Debug;
use std::rc::Rc;
use nom::combinator::map;
use nom::error::{ParseError, ErrorKind};
use nom::Err;
use nom::IResult;
use nom::branch::alt;
use nom::sequence::pair;
use std::collections::HashMap;

pub trait UnOp<O> = Fn(O) -> O;
pub trait BinOp<O> = Fn(O, O) -> O;
pub trait Parser<I, O> = Fn(I) -> IResult<I, O>;

fn throw_err_parser<I, O>() -> impl Parser<I, O> {
    box |input| {
        Err(Err::Error((input, ErrorKind::NoneOf)))
    }
}

fn unit<I, O>(output: O) -> impl Parser<I, O>
where I: Clone + 'static, O: Clone + 'static {
    move |input: I| Ok((input, output.clone()))
}

fn bind<I, Oa, Ob>(
    parse_a: Rc<impl Parser<I, Oa> + ?Sized>,
    binder: impl Fn(Oa) -> Box<dyn Parser<I, Ob>>
) -> impl Parser<I, Ob> {
    move |input: I| {
        let (input, a) = (parse_a)(input)?;
        binder(a)(input)
    }
}

// #[derive(Clone, Debug)]
// pub enum MixfixErrorKind {
//     NoParser,
//     GeneralPurpose,
// }

#[derive(PartialEq)]
enum Fixity {
    Prefix,
    Postfix,
    Infix,
    InfixL,
    InfixR,
}

enum Operator<I, O>
where I: Clone + 'static, O: Clone + 'static {
    UnOp(Fixity, Rc<dyn Parser<I, Box<dyn UnOp<O>>>>),
    BinOp(Fixity, Rc<dyn Parser<I, Box<dyn BinOp<O>>>>),
}

type Mixes<I, O>
where I: Clone + 'static, O: Clone + 'static
    = Vec<Operator<I, O>>;

// struct Mixes<I, O>
// where I: Clone + 'static, O: Clone + 'static {
//     ops:
// }

// impl<I, O> Mixes<I, O>
// where I: Clone + 'static, O: Clone + 'static {
//
//     pub fn parse_infix(&self) -> Option<Vec<>>
//
// }

pub struct MixfixParser<I, O>
where I: Clone + 'static, O: Clone + 'static
{
    terminals: Rc<Box<dyn Parser<I, O>>>,
    levels: HashMap<usize, Rc<Mixes<I, O>>>
}

impl<I, O> MixfixParser<I, O>
where I: Clone + 'static + std::fmt::Debug,
      O: Clone + 'static
{

    pub fn parse(&self, input: I) -> IResult<I, O> {
        self.mainloop(self.min_level())(input)
    }

    fn mainloop(&self, current: Option<usize>) -> Rc<dyn Parser<I, O>> {
        match current {
            None => {
                self.terminals_parser()
            },
            Some(curr) => {
                let curr_level = self.levels.get(&curr).unwrap();
                MixfixParser::build_level_directed(
                    curr_level.clone(),
                    MixfixParser::build_level_nondirected(
                        curr_level.clone(),
                        self.mainloop(self.next_level(curr))
                    )
                )
            }
        }
    }

    fn min_level(&self) -> Option<usize> {
        self.levels.keys().min().cloned()
    }

    fn next_level(&self, prev: usize) -> Option<usize> {
        self.levels.keys().filter(|curr| *curr > &prev).min().cloned()
    }

    fn parser_unop(&self, level: usize, fixity: Fixity) -> impl Parser<I, Box<dyn UnOp<O>>> {
        use crate::mixfix_vec::UnOp;
        use Operator::*; use Fixity::*;

        let parsers: Vec<Rc<dyn Parser<I, Box<dyn UnOp<O>>>>> =
            self.levels.get(&level).cloned().unwrap_or_default().iter()
                .map(|x| match x {
                    UnOp(Prefix, parser) => Some((*parser).clone()),
                    _ => None
                })
                .filter(|x| x.is_some()).map(|x| x.unwrap())
                .collect();

        move |input: I| {
            let input_fold = input.clone();
            parsers.clone().iter()
                .fold(
                    Err(Err::Error((input, ErrorKind::Alt))),
                    |acc, parse| {
                        if acc.is_ok() { return acc }
                        let res = parse(input_fold.clone());
                        if res.is_ok() { res } else { acc }
                    }
                )
        }
    }

    fn parser_binop(&self, level: usize, fixity: Fixity) -> impl Parser<I, Box<dyn BinOp<O>>> {
        use crate::mixfix_vec::BinOp;
        use Operator::*; use Fixity::*;

        let parsers: Vec<Rc<dyn Parser<I, Box<dyn BinOp<O>>>>> =
            self.levels.get(&level).cloned().unwrap_or_default().iter()
                .map(|x| match x {
                    BinOp(fix, parser) if fix == &fixity => Some((*parser).clone()),
                    _ => None
                })
                .filter(|x| x.is_some()).map(|x| x.unwrap())
                .collect();

        move |input: I| {
            let input_fold = input.clone();
            parsers.clone().iter()
                .fold(
                    Err(Err::Error((input, ErrorKind::Alt))),
                    |acc, parse| {
                        if acc.is_ok() { return acc }
                        let res = parse(input_fold.clone());
                        if res.is_ok() { res } else { acc }
                    }
                )
        }
    }

    fn prefix(&self, level: usize) -> impl Parser<I, UnOp>
    // fn postfix
    // fn infix
    // fn infix_l
    // fn infix_r

    fn terminals_parser(&self) -> Rc<dyn Parser<I, O>> {
        let terminals = Rc::clone(&self.terminals);
        Rc::new(
            move |i: I| terminals(i)
        )
    }

    fn build_level_nondirected(
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> Rc<dyn Parser<I, O>> {
        Rc::new(bind(next_level.clone(), move |x|
            box alt((
                MixfixParser::level_inf_after_one(
                    x.clone(),
                    mixes.clone(),
                    next_level.clone()
                ),
                unit(x)
            ))
        ))
    }

    fn build_level_directed(
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> Rc<dyn Parser<I, O>> {
        let mixes_c = mixes.clone();
        let next_level_c = next_level.clone();

        Rc::new(
            alt((
                bind(next_level.clone(), move |lhs|
                    box alt((
                        MixfixParser::level_infl_after_one(
                            lhs.clone(),
                            mixes.clone(),
                            next_level.clone()
                        ),
                        MixfixParser::level_infr_after_one(
                            lhs.clone(),
                            mixes.clone(),
                            next_level.clone()
                        ),
                        unit(lhs.clone())
                    ))
                ),
                MixfixParser::level_infr_not_after_one(
                    mixes_c.clone(),
                    next_level_c.clone()
                )
            ))
        )
    }

    fn level_inf_after_one(
        lhs: O,
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> impl Parser<I, O> {
        bind(Rc::clone(&mixes.infix), move |f| {
            let lhs = lhs.clone();
            box bind(next_level.clone(), move |rhs|
                box unit(f(lhs.clone(), rhs.clone()))
            )
        })
    }

    fn level_infl_after_one(
        lhs: O,
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> impl Parser<I, O> {
        let next_level_c = next_level.clone();
        let lhs_c = lhs.clone();
        let infixl_or_postfix =
            alt((
                bind(mixes.infix_l.clone(), move |f| {
                    let lhs = lhs.clone();
                    box bind(next_level.clone(), move |rhs|
                        box unit(f(lhs.clone(), rhs.clone()))
                    )
                }),
                bind(mixes.postfix.clone(), move |f| {
                    box unit(f(lhs_c.clone()))
                })
            ));

        bind(Rc::new(infixl_or_postfix), move |lhs_recur|
            box alt((
                MixfixParser::level_infl_after_one(
                    lhs_recur.clone(), mixes.clone(), next_level_c.clone()
                ),
                unit(lhs_recur.clone())
            ))
        )
    }

    fn level_infr_after_one (
        lhs: O,
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> impl Parser<I, O> {
        bind(mixes.infix_r.clone(), move |f| {
            let lhs = lhs.clone();
            box bind(next_level.clone(), move |rhs|
                box unit(f(lhs.clone(), rhs.clone()))
            )
        })
    }
    fn level_infr (
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> impl Parser<I, O> {
        let mixes_c = mixes.clone();
        let next_level_c = next_level.clone();

        let level_infr_after_one = bind(next_level.clone(), move |lhs|
            box alt((
                MixfixParser::level_infr_after_one(
                    lhs.clone(),
                    Rc::clone(&mixes_c),
                    Rc::clone(&next_level_c)
                ),
                unit(lhs.clone())
            ))
        );

        alt((
            level_infr_after_one,
            MixfixParser::level_infr_not_after_one(
                mixes.clone(),
                next_level.clone()
            )
        ))
    }

    fn level_infr_not_after_one (
        mixes: Rc<Mixes<I, O>>,
        next_level: Rc<dyn Parser<I, O> + 'static>
    ) -> impl Parser<I, O> {
        bind(mixes.prefix.clone(), move |f| {
            box bind(next_level.clone(), move |rhs|
                box unit(f(rhs.clone()))
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;
    use nom::number::complete::double;
    use nom::multi::fold_many0;
    use nom::bytes::complete::is_a;
    use nom::bytes::complete::tag as str_tag;
    use super::*;

    #[derive(Clone, Debug, PartialEq)]
    enum Expr {
        NumE(f64),
        PlusE(Box<Expr>, Box<Expr>),
        MultE(Box<Expr>, Box<Expr>),
        NegE(Box<Expr>)
    }

    #[derive(Clone, Debug, PartialEq)]
    pub enum Token {
        Space,
        Num(f64),
        Op(String),
    }

    fn lex(input: &str) -> IResult<&str, Vec<Token>> {
        let (input, tokbuf) = fold_many0(
            alt((
                map(alt((str_tag("+"), str_tag("-"), str_tag("*"), str_tag("/"))), |x: &str| Token::Op(x.into())),
                map(is_a(" \t\n\r"), |x| Token::Space),
                map(double, |i| Token::Num(i))
            )),
            Vec::new(),
            |mut acc: Vec<_>, tok| {
                acc.push(tok);
                acc
            }
        )(input)?;

        let tokbuf = tokbuf
            .into_iter()
            .filter(|x| *x != Token::Space)
            .collect();

        Ok((input, tokbuf))
    }

    fn take_num(input: Vec<Token>) -> IResult<Vec<Token>, Box<Expr>> {
        let mut input = input.clone();

        if input.len() == 0 {
            return Err(Err::Error((input, ErrorKind::Float)))
        };

        if let Token::Num(i) = input.remove(0) {
            return Ok((input, Box::new(Expr::NumE(i))))
        };

        Err(Err::Error((input, ErrorKind::Float)))
    }

    fn tag<'a>(tag: Token) -> impl Fn(Vec<Token>) -> IResult<Vec<Token>, Token> {
        move |input: Vec<Token>| {
            let mut input = input.clone();
            if input.len() == 0 {
                return Err(Err::Error((input, ErrorKind::Tag)))
            };

            if input.remove(0) == tag {
                return Ok((input, tag.clone()))
            };

            return Err(Err::Error((input, ErrorKind::Tag)))
        }
    }


    fn make_simple_parser() -> MixfixParser<Vec<Token>, Box<Expr>> {
        use Token::*;

        let mut levels = HashMap::new();
        levels.insert(1, Rc::new(Mixes {
            infix_l: Rc::new(box |input: Vec<Token>| {
                let (input, _) = tag(Op("+".into()))(input)?;
                let cb: Box<dyn BinOp<Box<Expr>>> =
                    box |lhs, rhs| box Expr::PlusE(lhs, rhs);
                Ok((input, cb))
            }),
            ..Mixes::default()
        }));

        levels.insert(2, Rc::new(Mixes {
            infix_l: Rc::new(box |input: Vec<Token>| {
                let (input, _) = tag(Op("*".into()))(input)?;
                let cb: Box<dyn BinOp<Box<Expr>>> =
                    box |lhs, rhs| box Expr::MultE(lhs, rhs);
                Ok((input, cb))
            }),
            ..Mixes::default()
        }));

        levels.insert(3, Rc::new(Mixes {
            prefix: Rc::new(box |input: Vec<Token>| {
                let (input, _) = tag(Op("-".into()))(input)?;
                let cb: Box<dyn UnOp<Box<Expr>>> =
                    box |rhs| box Expr::NegE(rhs);
                Ok((input, cb))
            }),
            ..Mixes::default()
        }));

        let mut mp = MixfixParser {
            terminals: Rc::new(box take_num),
            levels
        };

        mp
    }

    fn lex_and_parse(program: &str, debug: bool) -> IResult<Vec<Token>, Box<Expr>> {
        let mp = make_simple_parser();

        let (program, tokbuf) = lex(program).expect("couldn't lex program");
        if (debug) { println!("(program, tokbuf) = ({:?}, {:?})", program, tokbuf) }

        let (program, ast) = mp.parse(tokbuf)?;
        if (debug) { println!("(program, ast) = ({:?}, {:?})", program, ast) }

        Ok((program, ast))
    }

    #[test]
    fn test_mixfix_basic_plus() {
        use Expr::*;

        let program = "1 + 2 + 3";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(ast,
            box PlusE(
                box PlusE(
                    box NumE(1.0),
                    box NumE(2.0)
                ),
                box NumE(3.0))
        )
    }

    #[test]
    fn test_mixfix_basic_mult() {
        use Expr::*;

        let program = "1 * 2 * 3";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(ast,
            box MultE(
                box MultE(
                    box NumE(1.0),
                    box NumE(2.0)
                ),
                box NumE(3.0))
        )
    }

    #[test]
    fn test_mixfix_basic_plus_mult() {
        use Expr::*;

        let program = "1 + 2 * 3 + 4";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(ast,
            box PlusE(
                box PlusE(
                    box NumE(1.0),
                    box MultE(
                        box NumE(2.0),
                        box NumE(3.0)
                    )
                ),
                box NumE(4.0)
            )
        )
    }

    #[test]
    fn test_mixfix_basic_neg() {
        use Expr::*;

        let program = "-1";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(ast,
            box NegE(
                box NumE(1.0)
            )
        )
    }

    #[test]
    fn test_mixfix_basic_neg_plus() {
        use Expr::*;

        let program = "-1 + -3";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(ast,
            box PlusE(
                box NegE(
                    box NumE(1.0)
                ),
                box NegE(
                    box NumE(3.0)
                )
            )
        )
    }
}
