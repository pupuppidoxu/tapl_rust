use nom::{branch::alt, sequence::preceded, IResult};

use crate::syntax::{Context, Ty};

use super::{
    reserved::Reserved,
    util::{paren, ucid, ws},
};

type Out = Box<dyn FnOnce(&Context<()>) -> Ty>;

pub(super) fn type_expression(s: &str) -> IResult<&str, Out> {
    alt((type_all, type_arr))(s)
}

fn type_arr(s: &str) -> IResult<&str, Out> {
    fn rec(s: &str, to_ty1: Out) -> IResult<&str, Out> {
        match preceded(ws(Reserved::Arrow.to_tag()), type_atomic)(s) {
            Ok((s, to_ty1_next)) => {
                let (s, to_ty2) = rec(s, to_ty1_next)?;
                Ok((
                    s,
                    Box::new(|c| Ty::Arr(to_ty1(c).boxed(), to_ty2(c).boxed())),
                ))
            }
            Err(_) => Ok((s, to_ty1)),
        }
    }

    let (s, to_ty1) = type_atomic(s)?;
    rec(s, to_ty1)
}

fn type_atomic(s: &str) -> IResult<&str, Out> {
    fn type_bool(s: &str) -> IResult<&str, Out> {
        let (s, _) = Reserved::Bool.to_tag()(s)?;
        Ok((s, Box::new(|_| Ty::Bool)))
    }

    fn type_nat(s: &str) -> IResult<&str, Out> {
        let (s, _) = Reserved::Nat.to_tag()(s)?;
        Ok((s, Box::new(|_| Ty::Nat)))
    }

    fn type_var(s: &str) -> IResult<&str, Out> {
        let (s, name) = ucid(s)?;
        let name = name.to_owned();
        Ok((
            s,
            Box::new(move |c| {
                let index = c
                    .index_of(&name)
                    // TODO
                    .unwrap();
                Ty::Var(index, c.len())
            }),
        ))
    }

    alt((paren(type_expression), type_bool, type_nat, type_var))(s)
}

fn type_all(s: &str) -> IResult<&str, Out> {
    let (s, name) = preceded(Reserved::At.to_tag(), ucid)(s)?;
    let (s, to_ty2) = preceded(ws(Reserved::Comma.to_tag()), type_expression)(s)?;
    let name = name.to_owned();
    Ok((
        s,
        Box::new(|c| {
            let mut inner = c.clone();
            inner.add_name(name.clone());
            Ty::All(name, to_ty2(&inner).boxed())
        }),
    ))
}
