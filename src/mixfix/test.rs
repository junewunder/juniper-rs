#[cfg(test)]
mod tests {
    use crate::mixfix::mixfix::*;
    use nom::bytes::complete::is_a;
    use nom::bytes::complete::tag as str_tag;
    use nom::multi::fold_many0;
    use nom::number::complete::double;
    use std::error::Error;

    #[derive(Clone, Debug, PartialEq)]
    enum Expr {
        NumE(f64),
        PlusE(Box<Expr>, Box<Expr>),
        MultE(Box<Expr>, Box<Expr>),
        NegE(Box<Expr>),
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
                map(
                    alt((str_tag("+"), str_tag("-"), str_tag("*"), str_tag("/"))),
                    |x: &str| Token::Op(x.into()),
                ),
                map(is_a(" \t\n\r"), |x| Token::Space),
                map(double, |i| Token::Num(i)),
            )),
            Vec::new(),
            |mut acc: Vec<_>, tok| {
                acc.push(tok);
                acc
            },
        )(input)?;

        let tokbuf = tokbuf.into_iter().filter(|x| *x != Token::Space).collect();

        Ok((input, tokbuf))
    }

    fn take_num(input: Vec<Token>) -> IResult<Vec<Token>, Box<Expr>> {
        let mut input = input.clone();

        if input.len() == 0 {
            let e: ErrorKind = ErrorKind::Float;
            return Err(Err::Error(ParseError::from_error_kind(input, e)));
        };

        if let Token::Num(i) = input.remove(0) {
            return Ok((input, Box::new(Expr::NumE(i))));
        };

        let e: ErrorKind = ErrorKind::Float;
        Err(Err::Error(ParseError::from_error_kind(input, e)))
    }

    fn tag<'a>(tag: Token) -> impl Fn(Vec<Token>) -> IResult<Vec<Token>, Token> {
        move |input: Vec<Token>| {
            let mut input = input.clone();
            if input.len() == 0 {
                let e: ErrorKind = ErrorKind::Tag;
                return Err(Err::Error(ParseError::from_error_kind(input, e)));
            };

            if input.remove(0) == tag {
                return Ok((input, tag.clone()));
            };

            let e: ErrorKind = ErrorKind::Tag;
            Err(Err::Error(ParseError::from_error_kind(input, e)))
        }
    }

    fn make_simple_parser() -> MixfixParser<Vec<Token>, Box<Expr>> {
        use Token::*;

        let mut levels = HashMap::new();
        levels.insert(
            1,
            Rc::new(Mixes {
                infix_l: Rc::new(box |input: Vec<Token>| {
                    let (input, _) = tag(Op("+".into()))(input)?;
                    let cb: Box<dyn BinOp<Box<Expr>>> = box |lhs, rhs| box Expr::PlusE(lhs, rhs);
                    Ok((input, cb))
                }),
                ..Mixes::default()
            }),
        );

        levels.insert(
            2,
            Rc::new(Mixes {
                infix_l: Rc::new(box |input: Vec<Token>| {
                    let (input, _) = tag(Op("*".into()))(input)?;
                    let cb: Box<dyn BinOp<Box<Expr>>> = box |lhs, rhs| box Expr::MultE(lhs, rhs);
                    Ok((input, cb))
                }),
                ..Mixes::default()
            }),
        );

        levels.insert(
            3,
            Rc::new(Mixes {
                prefix: Rc::new(box |input: Vec<Token>| {
                    let (input, _) = tag(Op("-".into()))(input)?;
                    let cb: Box<dyn UnOp<Box<Expr>>> = box |rhs| box Expr::NegE(rhs);
                    Ok((input, cb))
                }),
                ..Mixes::default()
            }),
        );

        let mut mp = MixfixParser {
            terminals: Rc::new(box take_num),
            levels,
        };

        mp
    }

    fn lex_and_parse(program: &str, debug: bool) -> IResult<Vec<Token>, Box<Expr>> {
        let mp = make_simple_parser();

        let (program, tokbuf) = lex(program).expect("couldn't lex program");
        if (debug) {
            println!("(program, tokbuf) = ({:?}, {:?})", program, tokbuf)
        }

        let (program, ast) = mp.parse(tokbuf)?;
        if (debug) {
            println!("(program, ast) = ({:?}, {:?})", program, ast)
        }

        Ok((program, ast))
    }

    #[test]
    fn test_mixfix_basic_plus() {
        use Expr::*;

        let program = "1 + 2 + 3";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(
            ast,
            box PlusE(box PlusE(box NumE(1.0), box NumE(2.0)), box NumE(3.0))
        )
    }

    #[test]
    fn test_mixfix_basic_mult() {
        use Expr::*;

        let program = "1 * 2 * 3";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(
            ast,
            box MultE(box MultE(box NumE(1.0), box NumE(2.0)), box NumE(3.0))
        )
    }

    #[test]
    fn test_mixfix_basic_plus_mult() {
        use Expr::*;

        let program = "1 + 2 * 3 + 4";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(
            ast,
            box PlusE(
                box PlusE(box NumE(1.0), box MultE(box NumE(2.0), box NumE(3.0))),
                box NumE(4.0)
            )
        )
    }

    #[test]
    fn test_mixfix_basic_neg() {
        use Expr::*;

        let program = "-1";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(ast, box NegE(box NumE(1.0)))
    }

    #[test]
    fn test_mixfix_basic_neg_plus() {
        use Expr::*;

        let program = "-1 + -3";
        let (_, ast) = lex_and_parse(program, false).expect("err");

        assert_eq!(
            ast,
            box PlusE(box NegE(box NumE(1.0)), box NegE(box NumE(3.0)))
        )
    }
}
