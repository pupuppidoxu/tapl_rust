use anyhow::{bail, Result};

use super::{Binding, Context, Kind};

use Ty::*;

#[derive(Clone, Debug)]
pub(crate) enum Ty {
    Arr(Box<Ty>, Box<Ty>),
    Var(usize, usize),
    All(String, Kind, Box<Ty>),
    Abs(String, Kind, Box<Ty>),
    App(Box<Ty>, Box<Ty>),
    Bool,
    Nat,
}

impl Ty {
    pub(super) fn is_eqv_to(&self, other: &Self, ctx: &Context<Binding>) -> bool {
        match (self, other) {
            (Arr(s_ty1, s_ty2), Arr(o_ty1, o_ty2)) => {
                s_ty1.is_eqv_to(o_ty1, ctx) && s_ty2.is_eqv_to(o_ty2, ctx)
            }
            (Var(s_i, _), _) if ctx.is_ty_abb(*s_i) => {
                self.clone().simplify(ctx).is_eqv_to(other, ctx)
            }
            (_, Var(o_i, _)) if ctx.is_ty_abb(*o_i) => {
                self.is_eqv_to(&other.clone().simplify(ctx), ctx)
            }
            (Var(s_i, _), Var(o_i, _)) => s_i == o_i,
            (All(_, s_k1, s_ty2), All(_, o_k1, o_ty2)) => {
                let mut inner = ctx.clone();
                inner.add("_".to_string(), Binding::TyVar(Kind::Star));
                s_k1 == o_k1 && s_ty2.is_eqv_to(o_ty2, &inner)
            }
            (Abs(_, s_k1, s_ty2), Abs(_, o_k1, o_ty2)) => {
                let mut inner = ctx.clone();
                inner.add("_".to_string(), Binding::TyVar(Kind::Star));
                s_k1 == o_k1 && s_ty2.is_eqv_to(o_ty2, &inner)
            }
            (App(_, _), _) => self.clone().simplify(ctx).is_eqv_to(other, ctx),
            (_, App(_, _)) => self.is_eqv_to(&other.clone().simplify(ctx), ctx),
            (Bool, Bool) | (Nat, Nat) => true,
            _ => false,
        }
    }

    pub(crate) fn kind_in(&self, ctx: &Context<Binding>) -> Result<Kind> {
        match self {
            Arr(ty1, ty2) => {
                if ty1.kind_in(ctx)? != Kind::Star || ty2.kind_in(ctx)? != Kind::Star {
                    bail!("star kind expected")
                }
                Ok(Kind::Star)
            }
            Var(i, _) => ctx.kind_of(*i),
            All(name, k1, ty2) => {
                let mut inner = ctx.clone();
                inner.add(name.clone(), Binding::TyVar(k1.clone()));
                if ty2.kind_in(&inner)? != Kind::Star {
                    bail!("star kind expected")
                }
                Ok(Kind::Star)
            }
            Abs(name, k1, ty2) => {
                let mut inner = ctx.clone();
                inner.add(name.clone(), Binding::TyVar(k1.clone()));
                let k2 = ty2.kind_in(&inner)?;
                Ok(Kind::Arr(k1.clone().boxed(), k2.boxed()))
            }
            App(ty1, ty2) => {
                let k1 = ty1.kind_in(ctx)?;
                let k2 = ty2.kind_in(ctx)?;
                match k1 {
                    Kind::Arr(k11, k12) => {
                        if k2 == *k11 {
                            Ok(*k12)
                        } else {
                            bail!("parameter kind mismatch")
                        }
                    }
                    _ => bail!(
                        "arrow kind expected on {}",
                        ty1.to_string_in(&ctx.to_name_only())
                    ),
                }
            }
            Bool | Nat => Ok(Kind::Star),
        }
    }

    pub(crate) fn simplify(self, ctx: &Context<Binding>) -> Self {
        let mut res = self;
        loop {
            res = match res {
                App(ty1, ty2) => match *ty1 {
                    Abs(_, _, ty12) => ty12.subst_top(*ty2),
                    _ => App(ty1.simplify(ctx).boxed(), ty2),
                },
                Var(i, _) => match ctx.type_of(i) {
                    Ok(ty) => ty,
                    Err(_) => break,
                },
                _ => break,
            }
        }
        res
    }

    pub(super) fn subst_top(self, s: Self) -> Self {
        self.subst(0, s.shift(1)).shift(-1)
    }

    pub(super) fn subst(self, j: usize, s: Self) -> Self {
        self.map(
            |(i, n), c| {
                if i == c {
                    s.shift(c as isize)
                } else {
                    Var(i, n)
                }
            },
            j,
        )
    }

    pub(super) fn shift(self, d: isize) -> Self {
        self.shift_above(d, 0)
    }

    pub(super) fn shift_above(self, d: isize, c: usize) -> Self {
        self.map(
            |(i, n), c| {
                if i >= c {
                    Var(i.wrapping_add_signed(d), n.wrapping_add_signed(d))
                } else {
                    Var(i, n.wrapping_add_signed(d))
                }
            },
            c,
        )
    }

    fn map<F>(self, on_var: F, c: usize) -> Self
    where
        F: FnOnce((usize, usize), usize) -> Self + Clone,
    {
        match self {
            Arr(ty1, ty2) => Arr(
                ty1.map(on_var.clone(), c).boxed(),
                ty2.map(on_var, c).boxed(),
            ),
            Var(i, n) => on_var((i, n), c),
            All(name, k1, ty2) => All(name, k1, ty2.map(on_var, c + 1).boxed()),
            Abs(name, k1, ty2) => Abs(name, k1, ty2.map(on_var, c + 1).boxed()),
            App(ty1, ty2) => App(
                ty1.map(on_var.clone(), c).boxed(),
                ty2.map(on_var, c).boxed(),
            ),
            Bool | Nat => self,
        }
    }

    pub(crate) fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub(crate) fn to_string_in(&self, ctx: &Context<()>) -> String {
        match self {
            Arr(ty1, ty2) => format!("({} -> {})", ty1.to_string_in(ctx), ty2.to_string_in(ctx)),
            Var(i, n) => {
                let name = ctx.name_of(*i);
                if *n != ctx.len() {
                    unreachable!("bad index on {name}(n: {n}, len: {})", ctx.len());
                }
                name.to_string()
            }
            All(name, k1, ty2) => {
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                format!("(@{name}::{k1}, {})", ty2.to_string_in(&inner),)
            }
            Abs(name, k1, ty2) => {
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                format!("(\\\\{name}::{k1}, {})", ty2.to_string_in(&inner),)
            }
            App(ty1, ty2) => format!("{}[{}]", ty1.to_string_in(ctx), ty2.to_string_in(ctx)),
            Bool | Nat => format!("{self:?}"),
        }
    }
}
