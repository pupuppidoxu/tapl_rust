use Ty::*;

use super::Context;

#[derive(Clone, Debug)]
pub(crate) enum Ty {
    Arr(Box<Ty>, Box<Ty>),
    Var(usize, usize),
    All(String, Box<Ty>),
    Bool,
    Nat,
}

impl PartialEq for Ty {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Arr(s_ty1, s_ty2), Arr(o_ty1, o_ty2)) => s_ty1 == o_ty1 && s_ty2 == o_ty2,
            (Var(s_i, _), Var(o_i, _)) => s_i == o_i,
            (All(_, s_ty2), All(_, o_ty2)) => s_ty2 == o_ty2,
            (Bool, Bool) | (Nat, Nat) => true,
            _ => false,
        }
    }
}

impl Ty {
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
            All(name, ty2) => All(name, ty2.map(on_var, c + 1).boxed()),
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
                let name = ctx.name_of(*i).expect("wrong context");
                if *n != ctx.len() {
                    unreachable!("bad index on {name}(n: {n}, len: {})", ctx.len());
                }
                name.to_string()
            }
            All(name, ty1) => {
                let mut inner = ctx.clone();
                inner.add_name(name.clone());
                format!("(@{name}, {})", ty1.to_string_in(&inner),)
            }
            Bool | Nat => format!("{self:?}"),
        }
    }
}
