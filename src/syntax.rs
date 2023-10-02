mod term;
mod ty;

use anyhow::{bail, Context as _, Result};
use std::collections::VecDeque;

pub(crate) use self::{term::Term, ty::Ty};

#[derive(Clone, Debug)]
pub(crate) enum Binding {
    Var(Ty),
    TyVar,
}

impl Binding {
    fn shift(self, d: isize) -> Self {
        match self {
            Self::Var(ty) => Self::Var(ty.shift(d)),
            Self::TyVar => self,
        }
    }
}

#[derive(Clone)]
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

    fn name_of(&self, index: usize) -> Result<&str> {
        self.get(index).map(|(name, _)| name.as_str())
    }

    fn get(&self, index: usize) -> Result<&(String, B)> {
        self.0.get(index).context("var lookup failure")
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
        let (_, binding) = self.get(index)?;
        let binding = binding.clone().shift(index as isize + 1);
        match binding {
            Binding::Var(ty) => Ok(ty.clone()),
            _ => bail!("wrong binding for var"),
        }
    }
}
