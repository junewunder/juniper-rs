use crate::mixfix::mixfix::*;
use std::rc::Rc;

pub fn bin_op<I, IO, O>(
    p: impl Parser<I, IO> + 'static,
    f: Box<dyn BinOp<O>>,
) -> Box<dyn Parser<I, Box<dyn BinOp<O>>>>
where
    I: Clone + 'static,
    O: Clone + 'static,
{
    let f = Rc::new(f);
    box move |input: I| {
        let f = f.clone();
        let (input, _) = p(input)?;
        Ok((input, box move |lhs, rhs| f(lhs, rhs)))
    }
}

pub fn un_op<I, IO, O>(
    p: impl Parser<I, IO> + 'static,
    f: Box<dyn UnOp<O>>,
) -> Box<dyn Parser<I, Box<dyn UnOp<O>>>>
where
    I: Clone + 'static,
    O: Clone + 'static,
{
    let f = Rc::new(f);
    box move |input: I| {
        let f = f.clone();
        let (input, _) = p(input)?;
        Ok((input, box move |x| f(x)))
    }
}
