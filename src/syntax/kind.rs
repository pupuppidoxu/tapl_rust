use std::fmt::Display;

use Kind::*;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Kind {
    Star,
    Arr(Box<Kind>, Box<Kind>),
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Star => write!(f, "*")?,
            Arr(k1, k2) => write!(f, "({k1} => {k2})")?,
        }
        Ok(())
    }
}

impl Kind {
    pub(crate) fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}
