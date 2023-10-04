mod kind;
mod term;
mod ty;

use anyhow::{bail, Context as _, Result};
use std::collections::VecDeque;

pub(crate) use self::{kind::Kind, term::Term, ty::Ty};

#[derive(Clone, Debug)]
pub(crate) enum Binding {
    Var(Ty),
    TyVar(Kind),
    TyAbb(Ty, Kind),
}

impl Binding {
    fn shift(self, d: isize) -> Self {
        use Binding::*;

        match self {
            Var(ty) => Var(ty.shift(d)),
            TyVar(_) => self,
            TyAbb(ty, k) => TyAbb(ty.shift(d), k),
        }
    }
}

#[derive(Clone, Debug)]
pub(super) struct Context<B>(VecDeque<(String, B)>);

impl<B> Context<B> {
    pub(crate) fn new() -> Self {
        Context(VecDeque::new())
    }

    pub(crate) fn add(&mut self, name: String, b: B) {
        self.0.push_front((name, b))
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn index_of(&self, name: &str) -> Result<usize> {
        self.0
            .iter()
            .position(|e| e.0 == name)
            .with_context(|| format!("no bind for \"{name}\""))
    }

    fn name_of(&self, index: usize) -> &str {
        self.get(index).0.as_str()
    }

    fn get(&self, index: usize) -> &(String, B) {
        self.0.get(index).expect("wrong context")
    }

    pub(crate) fn to_name_only(&self) -> Context<()> {
        Context(self.0.iter().map(|(name, _)| (name.clone(), ())).collect())
    }
}

impl Context<()> {
    pub(crate) fn add_name(&mut self, name: String) {
        self.add(name, ())
    }
}

impl Context<Binding> {
    fn type_of(&self, index: usize) -> Result<Ty> {
        let binding = self.get(index).1.clone().shift(index as isize + 1);
        match binding {
            Binding::Var(ty) => Ok(ty),
            Binding::TyAbb(ty, _) => Ok(ty),
            _ => bail!("wrong binding for var"),
        }
    }

    fn kind_of(&self, index: usize) -> Result<Kind> {
        match &self.get(index).1 {
            Binding::TyVar(k) => Ok(k.clone()),
            Binding::TyAbb(_, k) => Ok(k.clone()),
            _ => bail!("wrong binding for ty_var"),
        }
    }

    fn is_ty_abb(&self, index: usize) -> bool {
        match &self.get(index).1 {
            Binding::TyAbb(_, _) => true,
            _ => false,
        }
    }
}
