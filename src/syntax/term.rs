use anyhow::{bail, Result};

use super::{Binding, Context, Kind, Ty};

use Term::*;

#[derive(Clone, Debug)]
pub(crate) enum Term {
    Var(usize, usize),
    Abs(String, Ty, Box<Term>),
    App(Box<Term>, Box<Term>),
    TyAbs(String, Kind, Box<Term>),
    TyApp(Box<Term>, Ty),
    Bool(bool),
    Nat(usize),
    If(Box<Term>, Box<Term>, Box<Term>),
    Succ(Box<Term>),
    Let(String, Box<Term>, Box<Term>),
}

impl Term {
    pub(crate) fn type_in(&self, ctx: &Context<Binding>) -> Result<Ty> {
        match self {
            Var(i, _) => ctx.type_of(*i),
            Abs(name, ty1, t2) => {
                if ty1.kind_in(ctx)? != Kind::Star {
                    bail!("star kind expected")
                }
                let mut inner = ctx.clone();
                inner.add(name.clone(), Binding::Var(ty1.clone()));
                let ty2 = t2.type_in(&inner)?;
                Ok(Ty::Arr(ty1.clone().boxed(), ty2.shift(-1).boxed()))
            }
            App(t1, t2) => {
                let ty1 = t1.type_in(ctx)?;
                let ty2 = t2.type_in(ctx)?;
                match ty1.simplify(ctx) {
                    Ty::Arr(ty11, ty12) => {
                        if ty11.is_eqv_to(&ty2, ctx) {
                            Ok(*ty12)
                        } else {
                            bail!(
                                "arg type mismatch on {}",
                                self.to_string_in(&ctx.to_name_only())
                            )
                        }
                    }
                    _ => bail!(
                        "arrow type expected on t1 of {}",
                        self.to_string_in(&ctx.to_name_only())
                    ),
                }
            }
            TyAbs(name, k1, t2) => {
                let mut inner = ctx.clone();
                inner.add(name.clone(), Binding::TyVar(k1.clone()));
                let ty2 = t2.type_in(&inner)?;
                Ok(Ty::All(name.clone(), k1.clone(), ty2.boxed()))
            }
            TyApp(t1, ty2) => {
                let ty1 = t1.type_in(ctx)?;
                let k2 = ty2.kind_in(ctx)?;
                match ty1.simplify(ctx) {
                    Ty::All(_, k11, ty12) => {
                        if k2 != k11 {
                            bail!("wrong kind")
                        };
                        Ok(ty12.subst_top(ty2.clone()))
                    }
                    _ => bail!(
                        "universal type expected on t1 of {}",
                        self.to_string_in(&ctx.to_name_only())
                    ),
                }
            }
            Bool(_) => Ok(Ty::Bool),
            Nat(_) => Ok(Ty::Nat),
            If(t1, t2, t3) => {
                let ty1 = t1.type_in(ctx)?;
                let ty2 = t2.type_in(ctx)?;
                let ty3 = t3.type_in(ctx)?;
                if !ty1.is_eqv_to(&Ty::Bool, ctx) {
                    bail!(
                        "bool type expected on condition of {}",
                        self.to_string_in(&ctx.to_name_only())
                    )
                }
                if !ty2.is_eqv_to(&ty3, ctx) {
                    bail!(
                        "same type expected on cases of {}",
                        self.to_string_in(&ctx.to_name_only())
                    )
                }
                Ok(ty2)
            }
            Succ(t1) => {
                let ty1 = t1.type_in(ctx)?;
                if ty1.is_eqv_to(&Ty::Nat, ctx) {
                    Ok(Ty::Nat)
                } else {
                    bail!(
                        "nat type expected on t1 of {}",
                        self.to_string_in(&ctx.to_name_only())
                    )
                }
            }
            Let(name, t1, t2) => {
                let ty1 = t1.type_in(ctx)?;
                let mut inner = ctx.clone();
                inner.add(name.clone(), Binding::Var(ty1));
                t2.type_in(&inner).map(|ty2| ty2.shift(-1))
            }
        }
    }

    pub(crate) fn eval(self, ctx: &Context<Binding>) -> Term {
        let mut t = self;
        while !t.is_val() {
            t = t.eval1(ctx);
        }
        t
    }

    fn eval1(self, ctx: &Context<Binding>) -> Term {
        match self {
            App(t1, t2) => match *t1 {
                Abs(_, _, t12) if t2.is_val() => t12.subst_top(*t2),
                _ => {
                    if t1.is_val() {
                        App(t1, t2.eval1(ctx).boxed())
                    } else {
                        App(t1.eval1(ctx).boxed(), t2)
                    }
                }
            },
            TyApp(t1, ty2) => match *t1 {
                TyAbs(_, _, t12) => t12.subst_type_top(ty2),
                _ => TyApp(t1.eval1(ctx).boxed(), ty2),
            },
            If(t1, t2, t3) => match *t1 {
                Bool(v) => {
                    if v {
                        *t2
                    } else {
                        *t3
                    }
                }
                _ => If(t1.eval1(ctx).boxed(), t2, t3),
            },
            Succ(t1) => match *t1 {
                Nat(v) => Nat(v + 1),
                _ => Succ(t1.eval1(ctx).boxed()),
            },
            Let(name, t1, t2) => {
                if t1.is_val() {
                    t2.subst_top(*t1)
                } else {
                    Let(name, t1.eval1(ctx).boxed(), t2)
                }
            }
            Var(_, _) | Abs(_, _, _) | TyAbs(_, _, _) | Bool(_) | Nat(_) => unreachable!("no rule"),
        }
    }

    fn is_val(&self) -> bool {
        match self {
            Abs(_, _, _) | TyAbs(_, _, _) | Bool(_) | Nat(_) => true,
            Var(_, _) | App(_, _) | TyApp(_, _) | If(_, _, _) | Succ(_) | Let(_, _, _) => false,
        }
    }

    /// `[0 -> s]self`
    fn subst_top(self, s: Self) -> Self {
        self.subst(0, s.shift(1)).shift(-1)
    }

    fn subst(self, j: usize, s: Self) -> Self {
        self.map(
            |(i, n), c| {
                if i == c {
                    s.shift(c as isize)
                } else {
                    Var(i, n)
                }
            },
            |ty, _| ty,
            j,
        )
    }

    fn subst_type_top(self, ty_s: Ty) -> Self {
        self.subst_type(0, ty_s.shift(1)).shift(-1)
    }

    fn subst_type(self, j: usize, ty_s: Ty) -> Self {
        self.map(|(i, n), _| Var(i, n), |ty, c| ty.subst(c, ty_s), j)
    }

    fn shift(self, d: isize) -> Self {
        self.map(
            |(i, n), c| {
                if i >= c {
                    Var(i.wrapping_add_signed(d), n.wrapping_add_signed(d))
                } else {
                    Var(i, n.wrapping_add_signed(d))
                }
            },
            |ty, c| ty.shift_above(d, c),
            0,
        )
    }

    fn map<F1, F2>(self, on_var: F1, on_type: F2, c: usize) -> Self
    where
        F1: FnOnce((usize, usize), usize) -> Self + Clone,
        F2: FnOnce(Ty, usize) -> Ty + Clone,
    {
        match self {
            Var(i, n) => on_var((i, n), c),
            Abs(name, ty1, t2) => Abs(
                name,
                on_type.clone()(ty1, c),
                t2.map(on_var, on_type, c + 1).boxed(),
            ),
            App(t1, t2) => App(
                t1.map(on_var.clone(), on_type.clone(), c).boxed(),
                t2.map(on_var, on_type, c).boxed(),
            ),
            TyAbs(name, k1, t2) => TyAbs(name, k1, t2.map(on_var, on_type, c + 1).boxed()),
            TyApp(t1, ty2) => TyApp(t1.map(on_var, on_type.clone(), c).boxed(), on_type(ty2, c)),
            Bool(_) | Nat(_) => self,
            If(t1, t2, t3) => If(
                t1.map(on_var.clone(), on_type.clone(), c).boxed(),
                t2.map(on_var.clone(), on_type.clone(), c).boxed(),
                t3.map(on_var, on_type, c).boxed(),
            ),
            Succ(t1) => Succ(t1.map(on_var, on_type, c).boxed()),
            Let(name, t1, t2) => Let(
                name,
                t1.map(on_var.clone(), on_type.clone(), c).boxed(),
                t2.map(on_var, on_type, c + 1).boxed(),
            ),
        }
    }

    pub(crate) fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub(crate) fn to_string_in(&self, ctx: &Context<()>) -> String {
        match self {
            Var(i, n) => {
                let name = ctx.name_of(*i);
                if *n != ctx.len() {
                    unreachable!("bad index on {name}(n: {n}, len: {})", ctx.len());
                }
                name.to_string()
            }
            Abs(name, ty1, t2) => {
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                format!(
                    "(\\{name}:{}. {})",
                    ty1.to_string_in(ctx),
                    t2.to_string_in(&inner)
                )
            }
            App(t1, t2) => format!("({} {})", t1.to_string_in(ctx), t2.to_string_in(ctx)),
            TyAbs(name, k1, t2) => {
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                format!("(\\\\{name}::{k1}. {})", t2.to_string_in(&inner))
            }
            TyApp(t1, ty2) => format!("{}[{}]", t1.to_string_in(ctx), ty2.to_string_in(ctx)),
            Bool(v) => v.to_string(),
            Nat(v) => v.to_string(),
            If(t1, t2, t3) => format!(
                "if {} {{ {} }} else {{ {} }}",
                t1.to_string_in(ctx),
                t2.to_string_in(ctx),
                t3.to_string_in(ctx)
            ),
            Succ(t1) => format!("(++{})", t1.to_string_in(ctx)),
            Let(name, t1, t2) => {
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                format!(
                    "let {name} = {} in {}",
                    t1.to_string_in(ctx),
                    t2.to_string_in(&inner)
                )
            }
        }
    }
}
