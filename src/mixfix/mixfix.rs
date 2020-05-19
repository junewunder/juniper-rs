#![feature(type_alias_impl_trait)]
#![allow(unused)]

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
    Box::new(|input| {
        let e: ErrorKind = ErrorKind::NoneOf;
        Err(Err::Error(ParseError::from_error_kind(input, e)))
    })
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

#[derive(Clone, Debug)]
pub enum Fixity {
    Prefix,
    Postfix,
    Infix,
    InfixL,
    InfixR,
}

pub struct Mixes<I, O>
where I: Clone + 'static, O: Clone + 'static
{
    pub prefix: Rc<Box<dyn Parser<I, Box<dyn UnOp<O>>>>>,
    pub postfix: Rc<Box<dyn Parser<I, Box<dyn UnOp<O>>>>>,
    pub infix: Rc<Box<dyn Parser<I, Box<dyn BinOp<O>>>>>,
    pub infix_l: Rc<Box<dyn Parser<I, Box<dyn BinOp<O>>>>>,
    pub infix_r: Rc<Box<dyn Parser<I, Box<dyn BinOp<O>>>>>,
}

impl<I, O> Mixes<I, O>
where I: Clone + 'static, O: Clone + 'static {
    pub fn new_infix_l(
        levels: &mut HashMap<usize, Rc<Mixes<I, O>>>,
        precedence: usize,
        parser: Rc<Box<dyn Parser<I, Box<dyn BinOp<O>>>>>,
    ) {
        levels.insert(precedence, Rc::new(Mixes {
            infix_l: parser,
            ..Mixes::default()
        }));
    }

    pub fn new_prefix(
        levels: &mut HashMap<usize, Rc<Mixes<I, O>>>,
        precedence: usize,
        parser: Rc<Box<dyn Parser<I, Box<dyn UnOp<O>>>>>,
    ) {
        levels.insert(precedence, Rc::new(Mixes {
            prefix: parser,
            ..Mixes::default()
        }));
    }
}

impl<I, O> Default for Mixes<I, O>
where I: Clone + 'static, O: Clone + 'static
{
    fn default() -> Self {
        Mixes {
            prefix: Rc::new(Box::new(throw_err_parser())),
            postfix: Rc::new(Box::new(throw_err_parser())),
            infix: Rc::new(Box::new(throw_err_parser())),
            infix_l: Rc::new(Box::new(throw_err_parser())),
            infix_r: Rc::new(Box::new(throw_err_parser())),
        }
    }

}

pub struct MixfixParser<I, O>
where I: Clone + 'static, O: Clone + 'static
{
    pub terminals: Rc<Box<dyn Parser<I, O>>>,
    pub levels: HashMap<usize, Rc<Mixes<I, O>>>
}

impl<I, O> MixfixParser<I, O>
where I: Clone + 'static + std::fmt::Debug,
      O: Clone + 'static
{

    pub fn parse(&self, input: I) -> IResult<I, O> {
        self.mainloop(self.min_level())(input)
    }

    pub fn build_parser(&self) -> Rc<dyn Parser<I, O>> {
        self.mainloop(self.min_level())
    }

    fn mainloop(&self, current: Option<usize>) -> Rc<dyn Parser<I, O>> {
        match current {
            None => {
                self.terminals_parser()
            },
            Some(curr) => {
                let curr_level = self.levels.get(&curr).unwrap();
                MixfixParser::build_level_directed(
                    Rc::clone(curr_level),
                    MixfixParser::build_level_nondirected(
                        Rc::clone(curr_level),
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
            let binder = MixfixParser::level_infr(mixes.clone(), next_level.clone());
            box bind(Rc::new(binder), move |rhs|
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
            let e: ErrorKind = ErrorKind::Float;
            return Err(Err::Error(ParseError::from_error_kind(input, e)))
        };

        if let Token::Num(i) = input.remove(0) {
            return Ok((input, Box::new(Expr::NumE(i))))
        };

        let e: ErrorKind = ErrorKind::Float;
        Err(Err::Error(ParseError::from_error_kind(input, e)))
    }

    fn tag<'a>(tag: Token) -> impl Fn(Vec<Token>) -> IResult<Vec<Token>, Token> {
        move |input: Vec<Token>| {
            let mut input = input.clone();
            if input.len() == 0 {
                let e: ErrorKind = ErrorKind::Tag;
                return Err(Err::Error(ParseError::from_error_kind(input, e)))
            };

            if input.remove(0) == tag {
                return Ok((input, tag.clone()))
            };

            let e: ErrorKind = ErrorKind::Tag;
            Err(Err::Error(ParseError::from_error_kind(input, e)))
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
