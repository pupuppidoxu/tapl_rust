use nom::{bytes::complete::tag, error::ParseError, IResult};
use strum::{AsRefStr, EnumVariantNames};

#[derive(AsRefStr, EnumVariantNames)]
#[strum(serialize_all = "lowercase")]
pub(super) enum Reserved {
    // lowercase
    Let,
    In,
    True,
    False,
    If,
    Else,

    // uppercase
    #[strum(serialize = "Bool")]
    Bool,
    #[strum(serialize = "Nat")]
    Nat,

    // symbol
    #[strum(serialize = ".")]
    Dot,
    #[strum(serialize = ",")]
    Comma,
    #[strum(serialize = "\\")]
    Back,
    #[strum(serialize = "\\\\")]
    BackBack,
    #[strum(serialize = "#")]
    Sharp,
    #[strum(serialize = ":")]
    Colon,
    #[strum(serialize = ";")]
    Semi,
    #[strum(serialize = "=")]
    Eq,
    #[strum(serialize = "->")]
    Arrow,
    #[strum(serialize = "//")]
    SlashSlash,
    #[strum(serialize = "++")]
    PlusPlus,
    #[strum(serialize = "@")]
    At,
}

impl Reserved {
    pub(super) fn to_tag<'a, E: ParseError<&'a str> + 'a>(
        &'a self,
    ) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E> {
        tag(self.as_ref())
    }
}
