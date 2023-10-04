use nom::{
    branch::alt,
    combinator::{opt, value},
    sequence::preceded,
    IResult,
};

use crate::syntax::Kind;

use super::{reserved::Reserved, util::paren};

pub(super) fn opt_kind(s: &str) -> IResult<&str, Kind> {
    let (s, res) = opt(preceded(Reserved::ColonColon.to_tag(), kind_expression))(s)?;
    Ok((s, res.unwrap_or(Kind::Star)))
}

fn kind_expression(s: &str) -> IResult<&str, Kind> {
    arr_kind(s)
}

fn arr_kind(s: &str) -> IResult<&str, Kind> {
    fn rec(s: &str, k1: Kind) -> IResult<&str, Kind> {
        match preceded(Reserved::DArrow.to_tag(), kind_atomic)(s) {
            Ok((s, k1_next)) => {
                let (s, k2) = rec(s, k1_next)?;
                Ok((s, Kind::Arr(k1.boxed(), k2.boxed())))
            }
            Err(_) => Ok((s, k1)),
        }
    }

    let (s, k1) = kind_atomic(s)?;
    rec(s, k1)
}

fn kind_atomic(s: &str) -> IResult<&str, Kind> {
    alt((
        paren(kind_expression),
        value(Kind::Star, Reserved::Star.to_tag()),
    ))(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Kind::*;

    #[test]
    fn test_arr_kind() {
        assert_eq!(
            Ok((
                " rest",
                Arr(Star.boxed(), Arr(Star.boxed(), Star.boxed()).boxed())
            ),),
            arr_kind("*=>*=>* rest")
        );
    }
}
