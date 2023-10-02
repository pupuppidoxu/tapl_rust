use nom::{
    branch::alt,
    character::complete::{
        alpha1, alphanumeric1, char, digit1, line_ending, multispace0, not_line_ending,
    },
    combinator::{map_res, opt, recognize, value, verify},
    error::ParseError,
    multi::many0_count,
    sequence::{delimited, pair},
    IResult, Parser,
};
use strum::VariantNames;

use super::reserved::Reserved;

pub(super) fn ws<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

pub(super) fn ignored(s: &str) -> IResult<&str, ()> {
    value(
        (),
        pair(multispace0, many0_count(pair(comment, multispace0))),
    )(s)
}

fn comment(s: &str) -> IResult<&str, ()> {
    value(
        (),
        delimited(Reserved::SlashSlash.to_tag(), not_line_ending, line_ending),
    )(s)
}

pub(super) fn paren<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(char('('), ws(inner), char(')'))
}

pub(super) fn curly<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(char('{'), ws(inner), char('}'))
}

pub(super) fn lcid(s: &str) -> IResult<&str, &str> {
    verify(id, |name: &str| {
        name.chars().next().map_or(false, |c| c.is_lowercase())
    })(s)
}

pub(super) fn ucid(s: &str) -> IResult<&str, &str> {
    verify(id, |name: &str| {
        name.chars().next().map_or(false, |c| c.is_uppercase())
    })(s)
}

fn id(s: &str) -> IResult<&str, &str> {
    verify(recognize(pair(alpha1, opt(alphanumeric1))), |name| {
        !Reserved::VARIANTS.contains(name)
    })(s)
}

pub(super) fn boolean(s: &str) -> IResult<&str, bool> {
    map_res(
        alt((Reserved::True.to_tag(), Reserved::False.to_tag())),
        |r: &str| r.parse(),
    )(s)
}

pub(super) fn nat(s: &str) -> IResult<&str, usize> {
    map_res(digit1, |r: &str| r.parse())(s)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_comment() {
        assert_eq!(Ok((" rest", ())), comment("//comment\n rest"));
    }

    #[test]
    fn test_lcid() {
        assert_eq!(Ok((" rest", "l0U1")), lcid("l0U1 rest"));
        assert!(lcid("U").is_err());
    }

    #[test]
    fn test_boolean() {
        assert_eq!(Ok((" rest", true)), boolean("true rest"));
    }

    #[test]
    fn test_nat() {
        assert_eq!(Ok((" rest", 10)), nat("010 rest"));
    }
}
