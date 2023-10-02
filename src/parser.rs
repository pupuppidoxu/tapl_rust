mod reserved;
mod term_parser;
mod type_parser;
mod util;

use nom::{
    combinator::all_consuming,
    multi::many1,
    sequence::{delimited, terminated},
    Finish, IResult,
};

use crate::syntax::{Context, Term};

use self::{
    reserved::Reserved,
    term_parser::term_expression,
    util::{ignored, ws},
};

pub(super) enum Statement {
    Eval(Term),
}

type Out<T> = Box<dyn FnOnce(&Context<()>) -> T>;

pub(super) fn parse(s: &str) -> Result<Vec<Statement>, String> {
    let ctx = Context::new();
    program(s)
        .finish()
        .map(|(_, to_stmts)| to_stmts(&ctx))
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

    delimited(ignored, ws(stmt_eval), Reserved::Semi.to_tag())(s)
}
