use crate::data::Expr;
use crate::parse::types::*;
use nom::IResult;
use nom::{
    error::{ErrorKind, ParseError},
    Err,
};
use std::ops::Deref;

#[derive(Clone, Debug, PartialEq)]
pub struct Annotated<T> {
    pub tok: T,
    pub idx: usize,
    pub len: usize,
    // TODO: add filename
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
}

pub fn anno_prefix_parser<T, F>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PostAnnoUnOp<Expr>>>
where
    F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PreAnnoUnOp<Expr>>>,
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

        let cb_post: Box<dyn PostAnnoUnOp<Expr>> = box move |x: Box<Annotated<Expr>>| {
            let len = x.idx + x.len - idx;
            let tok = (cb_pre)(x);
            box Annotated { tok, idx, len }
        };
        Ok((input, cb_post))
    }
}

pub fn anno_postfix_parser<T, F>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PostAnnoUnOp<Expr>>>
where
    F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PreAnnoUnOp<Expr>>>,
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

        let cb_post: Box<dyn PostAnnoUnOp<Expr>> = box move |x: Box<Annotated<Expr>>| {
            let idx = x.idx;
            let len = post_idx - idx + post_len;
            let tok = (cb_pre)(x);
            box Annotated { tok, idx, len }
        };
        Ok((input, cb_post))
    }
}

pub fn anno_infix_parser<T, F>(
    parser: F,
) -> impl Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PostAnnoBinOp<Expr>>>
where
    F: Fn(Vec<Annotated<T>>) -> IResult<Vec<Annotated<T>>, Box<dyn PreAnnoBinOp<Expr>>>,
{
    move |input| {
        let (input, cb_pre) = parser(input)?;
        let cb_post: Box<dyn PostAnnoBinOp<Expr>> = box move |lhs, rhs| {
            let idx = lhs.idx;
            let len = rhs.idx - idx + rhs.len;
            let tok = (cb_pre)(lhs, rhs);
            box Annotated { tok, idx, len }
        };

        Ok((input, cb_post))
    }
}