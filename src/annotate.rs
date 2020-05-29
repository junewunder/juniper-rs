use crate::data::Expr;
use crate::parse::shared::*;
use nom::IResult;
use nom::{
    error::{ErrorKind, ParseError},
    Err,
};
use std::fmt;
use std::ops::Deref;

#[derive(Clone, PartialEq)]
pub struct Annotated<T> {
    pub tok: T,
    pub idx: usize,
    pub len: usize,
}

impl<T> From<T> for Annotated<T> {
    fn from(tok: T) -> Self {
        Annotated::zero(tok)
    }
}

impl<T> Annotated<T> {
    pub fn zero(tok: T) -> Annotated<T> {
        Annotated {
            tok,
            idx: 0,
            len: 0,
        }
    }

    pub fn unwrap(self) -> T {
        self.tok
    }

    pub fn cloned(&self) -> T
    where T: Clone {
        self.tok.clone()
    }
}

impl<T: fmt::Debug> fmt::Debug for Annotated<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.tok)
    }
}

impl<T: fmt::Display> fmt::Display for Annotated<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.tok)
    }
}

pub fn annotated_terminal<T, F, O>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<Annotated<O>>>
where F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, O>
{
    move |input: Vec<Annotated<T>>| {
        let idx = input.first().map(|x| x.idx);
        let last_idx_len = input.last().map(|x| x.idx + x.len);
        if idx.is_none() || last_idx_len.is_none() {
            return Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        let idx = idx.unwrap();
        let len = last_idx_len.unwrap() - idx;

        let (input, tok) = parser(input)?;
        let len = input.first().map(|x| x.idx - idx).unwrap_or(len);
        Ok((input, box Annotated { tok, idx, len }))
    }
}

pub fn anno_prefix_parser<T, F, O: 'static>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PostAnnoUnOp<O>>>
where
    F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PreAnnoUnOp<O>>>,
{
    move |input| {
        let idx = input.first().map(|x| x.idx);
        if idx.is_none() {
            return Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        let idx = idx.unwrap();

        let (input, cb_pre) = parser(input)?;

        let cb_post: Box<dyn PostAnnoUnOp<O>> = box move |x: Box<Annotated<O>>| {
            let len = x.idx + x.len - idx;
            let tok = (cb_pre)(x);
            box Annotated { tok, idx, len }
        };
        Ok((input, cb_post))
    }
}

pub fn anno_postfix_parser<T, F, O: 'static>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PostAnnoUnOp<O>>>
where
    F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PreAnnoUnOp<O>>>,
{
    move |input| {
        let post_idx = input.first().map(|x| x.idx);
        let post_len = input.first().map(|x| x.len);
        if post_idx.is_none() || post_len.is_none() {
            return Err(Err::Error(ParseError::from_error_kind(
                input,
                ErrorKind::Tag,
            )));
        }
        let post_idx = post_idx.unwrap();
        let post_len = post_len.unwrap();

        let (input, cb_pre) = parser(input)?;

        let cb_post: Box<dyn PostAnnoUnOp<O>> = box move |x: Box<Annotated<O>>| {
            let idx = x.idx;
            let len = post_idx - idx + post_len;
            let tok = (cb_pre)(x);
            box Annotated { tok, idx, len }
        };
        Ok((input, cb_post))
    }
}

pub fn anno_infix_parser<T, F, O: 'static>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PostAnnoBinOp<O>>>
where
    F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PreAnnoBinOp<O>>>,
{
    move |input| {
        let (input, cb_pre) = parser(input)?;
        let cb_post: Box<dyn PostAnnoBinOp<O>> = box move |lhs, rhs| {
            let idx = lhs.idx;
            let len = rhs.idx - idx + rhs.len;
            let tok = (cb_pre)(lhs, rhs);
            box Annotated { tok, idx, len }
        };

        Ok((input, cb_post))
    }
}
