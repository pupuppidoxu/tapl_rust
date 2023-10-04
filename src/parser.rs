mod kind_parser;
mod reserved;
mod term_parser;
mod type_parser;
mod util;

use nom::{
    branch::alt,
    character::complete::multispace1,
    combinator::all_consuming,
    multi::many1,
    sequence::{delimited, pair, preceded, terminated},
    Finish, IResult,
};

use crate::syntax::{Context, Term, Ty};

use self::{
    reserved::Reserved,
    term_parser::term_expression,
    type_parser::type_expression,
    util::{ignored, ucid, ws},
};

pub(super) enum Statement {
    Eval(Term),
    TyAbbBind(String, Ty),
}

type Out<T> = Box<dyn FnOnce(&mut Context<()>) -> T>;

pub(super) fn parse(s: &str) -> Result<Vec<Statement>, String> {
    let mut ctx = Context::new();
    program(s)
        .finish()
        .map(|(_, to_stmts)| to_stmts(&mut ctx))
        .map_err(|e| e.to_string())
}

fn program(s: &str) -> IResult<&str, Out<Vec<Statement>>> {
    let (s, out) = all_consuming(terminated(many1(statement), ignored))(s)?;
    Ok((
        s,
        Box::new(|c| out.into_iter().map(|to_stmt| to_stmt(c)).collect()),
    ))
}

fn statement(s: &str) -> IResult<&str, Out<Statement>> {
    fn stmt_eval(s: &str) -> IResult<&str, Out<Statement>> {
        let (s, to_t) = term_expression(s)?;
        Ok((s, Box::new(|c| Statement::Eval(to_t(c)))))
    }

    fn stmt_ty_abb_bind(s: &str) -> IResult<&str, Out<Statement>> {
        let (s, name) = preceded(pair(Reserved::Typealias.to_tag(), multispace1), ucid)(s)?;
        let (s, to_ty) = preceded(ws(Reserved::Eq.to_tag()), type_expression)(s)?;
        let name = name.to_owned();
        Ok((
            s,
            Box::new(|c| {
                let ty = to_ty(c);
                c.add_name(name.clone());
                Statement::TyAbbBind(name, ty)
            }),
        ))
    }

    delimited(
        ignored,
        ws(alt((stmt_ty_abb_bind, stmt_eval))),
        Reserved::Semi.to_tag(),
    )(s)
}
