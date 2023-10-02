mod parser;
mod syntax;

use crate::{
    parser::{parse, Statement},
    syntax::{Binding, Context, Term},
};

pub fn run(s: &str) {
    let ctx = Context::new();
    match parse(s) {
        Ok(terms) => terms.into_iter().for_each(|stmt| match stmt {
            Statement::Eval(t) => {
                print!("*Parse >> ");
                printing::print_term(&t, &ctx.to_name_only());
                print!("  Type >> ");
                if printing::check_type_while_printing(&t, &ctx) {
                    print!("  Eval >> ");
                    println!("_ = {}", t.eval(&ctx).to_string_in(&ctx.to_name_only()))
                }
            }
        }),
        Err(e) => println!("parse error: {e}"),
    }
}

mod printing {

    use anyhow::Error;

    use super::*;

    const NEW_LINE: &str = "\n        > ";

    pub(super) fn print_term(t: &Term, ctx: &Context<()>) {
        match t {
            Term::Let(name, t1, t2) => {
                print!("let {name} = {} in{NEW_LINE}", t1.to_string_in(ctx),);
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                print_term(t2, &inner)
            }
            _ => println!("{}", t.to_string_in(ctx)),
        }
    }

    pub(super) fn check_type_while_printing(t: &Term, ctx: &Context<Binding>) -> bool {
        fn print_err(e: Error) -> bool {
            println!("!!ERROR!! {e}");
            false
        }

        match t {
            Term::Let(name, t1, t2) => match t1.type_in(ctx) {
                Ok(ty1) => {
                    print!(
                        "{name}: {}{NEW_LINE}",
                        ty1.to_string_in(&ctx.to_name_only())
                    );
                    let mut inner = ctx.clone();
                    inner.add(name.clone(), Binding::Var(ty1));
                    check_type_while_printing(&t2, &inner)
                }
                Err(e) => print_err(e),
            },
            _ => match t.type_in(ctx) {
                Ok(ty) => {
                    println!("_: {}", ty.to_string_in(&ctx.to_name_only()));
                    true
                }
                Err(e) => print_err(e),
            },
        }
    }
}
