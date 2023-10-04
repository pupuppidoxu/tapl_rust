use nom::{
    branch::alt,
    character::complete::{multispace0, multispace1},
    sequence::{pair, preceded, terminated},
    IResult,
};

use crate::syntax::{Context, Term};

use super::{
    kind_parser::opt_kind,
    reserved::Reserved,
    type_parser::type_expression,
    util::{boolean, curly, lcid, nat, paren, square, ucid, ws},
};

type Out = Box<dyn FnOnce(&Context<()>) -> Term>;

pub(super) fn term_expression(s: &str) -> IResult<&str, Out> {
    alt((
        term_abs,
        term_ty_abs,
        term_if,
        term_succ,
        term_let,
        app_term,
    ))(s)
}

fn term_abs(s: &str) -> IResult<&str, Out> {
    let (s, name) = preceded(Reserved::Back.to_tag(), lcid)(s)?;
    let (s, to_ty1) = preceded(ws(Reserved::Colon.to_tag()), type_expression)(s)?;
    let (s, to_t2) = preceded(ws(Reserved::Dot.to_tag()), term_expression)(s)?;
    let name = name.to_owned();
    Ok((
        s,
        Box::new(move |c| {
            let mut inner = c.clone();
            inner.add_name(name.clone());
            Term::Abs(name, to_ty1(c), to_t2(&inner).boxed())
        }),
    ))
}

fn app_term(s: &str) -> IResult<&str, Out> {
    type Next = Box<dyn FnOnce(Out) -> Out>;

    fn term_app(s: &str) -> IResult<&str, Next> {
        let (s, to_t2) = preceded(multispace1, term_atomic)(s)?;
        Ok((
            s,
            Box::new(|to_t1| Box::new(|c: &_| Term::App(to_t1(c).boxed(), to_t2(c).boxed()))),
        ))
    }

    fn term_ty_app(s: &str) -> IResult<&str, Next> {
        let (s, to_ty2) = preceded(multispace0, square(type_expression))(s)?;
        Ok((
            s,
            Box::new(|to_t1| Box::new(|c: &_| Term::TyApp(to_t1(c).boxed(), to_ty2(c)))),
        ))
    }

    /// t1 t2 t3 == (t1 t2) t3
    fn rec(s: &str, to_t1: Out) -> IResult<&str, Out> {
        match alt((term_ty_app, term_app))(s) {
            Ok((s, next)) => rec(s, next(to_t1)),
            Err(_) => Ok((s, to_t1)),
        }
    }

    let (s, to_t1) = term_atomic(s)?;
    rec(s, to_t1)
}

fn term_atomic(s: &str) -> IResult<&str, Out> {
    fn term_bool(s: &str) -> IResult<&str, Out> {
        let (s, b) = boolean(s)?;
        Ok((s, Box::new(move |_| Term::Bool(b))))
    }

    fn term_nat(s: &str) -> IResult<&str, Out> {
        let (s, n) = nat(s)?;
        Ok((s, Box::new(move |_| Term::Nat(n))))
    }

    fn term_var(s: &str) -> IResult<&str, Out> {
        let (s, name) = lcid(s)?;
        let name = name.to_owned();
        Ok((
            s,
            Box::new(move |c| {
                let index = c
                    .index_of(&name)
                    // TODO
                    .unwrap();
                Term::Var(index, c.len())
            }),
        ))
    }

    alt((paren(term_expression), term_bool, term_nat, term_var))(s)
}

fn term_ty_abs(s: &str) -> IResult<&str, Out> {
    let (s, name) = preceded(Reserved::BackBack.to_tag(), ucid)(s)?;
    let (s, k1) = opt_kind(s)?;
    let (s, to_t2) = preceded(ws(Reserved::Dot.to_tag()), term_expression)(s)?;
    let name = name.to_owned();
    Ok((
        s,
        Box::new(move |c| {
            let mut inner = c.clone();
            inner.add_name(name.clone());
            Term::TyAbs(name, k1, to_t2(&inner).boxed())
        }),
    ))
}

fn term_if(s: &str) -> IResult<&str, Out> {
    let (s, _) = pair(Reserved::If.to_tag(), multispace1)(s)?;
    let (s, to_t1) = terminated(term_expression, multispace0)(s)?;
    let (s, to_t2) = curly(term_expression)(s)?;
    let (s, _) = ws(Reserved::Else.to_tag())(s)?;
    let (s, to_t3) = curly(term_expression)(s)?;
    Ok((
        s,
        Box::new(|c| Term::If(to_t1(c).boxed(), to_t2(c).boxed(), to_t3(c).boxed())),
    ))
}

fn term_succ(s: &str) -> IResult<&str, Out> {
    let (s, to_t1) = preceded(Reserved::PlusPlus.to_tag(), term_atomic)(s)?;
    Ok((s, Box::new(|c| Term::Succ(to_t1(c).boxed()))))
}

fn term_let(s: &str) -> IResult<&str, Out> {
    let (s, _) = pair(Reserved::Let.to_tag(), multispace1)(s)?;
    let (s, name) = lcid(s)?;
    let (s, _) = ws(Reserved::Eq.to_tag())(s)?;
    let (s, to_t1) = terminated(term_expression, multispace1)(s)?;
    let (s, _) = pair(Reserved::In.to_tag(), multispace1)(s)?;
    let (s, to_t2) = term_expression(s)?;
    let name = name.to_owned();
    Ok((
        s,
        Box::new(|c| {
            let mut inner = c.clone();
            inner.add_name(name.clone());
            Term::Let(name, to_t1(c).boxed(), to_t2(&inner).boxed())
        }),
    ))
}
