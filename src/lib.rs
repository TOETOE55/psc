pub mod core;

pub use crate::core::{
    combinators::{
        common::{Char, char, Satisfy, satisfy, Strg, strg, Regex, reg},
        eof, EOF, extra, failure, Failure, fix, Fix,
        ops::{ParseFn, ParserWrapper},
        pure,
    },
    err::ParseMsg,
    state::{ParseState, Pos},
    traits::{covert, parser::Parser, stream::Stream},
};

#[cfg(test)]
mod tests {

    use crate::core::combinators::Fix;
    use crate::{char, fix, pure, satisfy, strg, ParseFn, ParseMsg, ParseState, Parser, Stream, reg};

    fn num<'a>() -> impl Parser<ParseState<'a>, Target = u64> {
        reg("[0-9]+")
            .map(str::parse::<u64>)
            .map(Result::unwrap)
    }

    fn dynamic<'a>() -> impl Parser<ParseState<'a>, Target = char> {
        num().and_then(|n| {
            if n % 2 == 0 {
                Box::new(satisfy(|ch: &char| ch.is_uppercase()))
                    as Box<dyn Parser<ParseState<'a>, Target = char>>
            } else {
                Box::new(satisfy(|ch: &char| ch.is_lowercase()))
            }
        })
    }

    fn recursion<S: Stream<Item = char> + Clone>() -> impl Parser<S, Target = ()> {
        fix(|this| {
            Box::new(
                satisfy(|ch: &char| ch.is_uppercase())
                    .and_r(this)
                    .or(satisfy(|ch: &char| ch.is_lowercase())),
            )
        })
        .and_r(pure(|| {}))
    }

    fn parse_fn<S: Stream<Item = char> + Clone>(stream: &mut S) -> Result<(), ParseMsg> {
        let parser = (satisfy(|ch: &char| ch.is_uppercase()).wrap() >> ParseFn(parse_fn))
            | satisfy(|ch: &char| ch.is_lowercase()).wrap() >> (pure(|| {}));
        parser.parse(stream)
    }

    #[test]
    fn it_works() -> Result<(), ParseMsg> {
        let mut src = ParseState::new("11112");
        let parser = char('1').many();
        let res = parser.parse(&mut src)?;
        assert_eq!(res, vec!['1'; 4]);
        assert_eq!(src.src.as_str(), "2");

        let mut src = "1111".chars();
        let parser = char('1').many();
        let res = parser.parse(&mut src)?;
        assert_eq!(res, vec!['1'; 4]);
        assert_eq!(src.as_str(), "");

        let mut src = "111102".chars();
        let parser = fix(Box::new(Fix::coerce(|it| {
            Box::new(char('1').and_r(it).or(char('0')))
        })));
        let res = parser.parse(&mut src)?;
        assert_eq!(res, '0');
        assert_eq!(src.as_str(), "2");

        let mut src = ParseState::new("abcd");
        let res = char('a').and_r(strg("bcd")).parse(&mut src)?;
        assert_eq!(res, "bcd");
        assert_eq!(src.src.as_str(), "");

        let mut src = ParseState::new("1234");
        let res = num().parse(&mut src)?;
        assert_eq!(res, 1234);
        assert_eq!(src.as_str(), "");

        let mut src = ParseState::new("2H");
        let res = dynamic().parse(&mut src)?;
        assert_eq!(res, 'H');
        assert_eq!(src.as_str(), "");

        let mut src = ParseState::new("OIHFa");
        let res = recursion().parse(&mut src)?;
        assert_eq!(res, ());
        assert_eq!(src.as_str(), "");

        let mut src = "OIHFa".chars();
        let res = ParseFn(parse_fn).parse(&mut src)?;
        assert_eq!(res, ());
        assert_eq!(src.as_str(), "");

        let mut src = "123".chars();
        let parser = pure(|| vec![])
            .snoc(char('1'))
            .snoc(char('2'))
            .snoc(char('3'));
        let res = parser.parse(&mut src)?;
        assert_eq!(res, vec!['1', '2', '3']);
        assert_eq!(src.as_str(), "");

        let mut src = "OIHFa".chars();
        let res = pure(|| recursion()).join().parse(&mut src)?;
        assert_eq!(res, ());
        assert_eq!(src.as_str(), "");

        let mut src = "1122".chars();
        let res = char('1').many().chain(char('2').many()).parse(&mut src)?;
        assert_eq!(res, vec!['1', '1', '2', '2']);
        assert_eq!(src.as_str(), "");

        let mut src = "1122".chars();
        let parser = char('+')
            .or(char('-'))
            .tries()
            .map(|sig| sig.unwrap_or('+'))
            .cons(satisfy(|ch: &char| ch.is_numeric()).many())
            .map(Vec::into_iter)
            .map(Iterator::collect::<String>)
            .map(|s| s.parse::<i64>())
            .map(Result::unwrap);
        let res = parser.parse(&mut src)?;
        assert_eq!(res, 1122);
        assert_eq!(src.as_str(), "");

        let mut src = ParseState::new("23");
        let parser = char('1').wrap() >> char('1') | char('2');
        let res = parser.parse(&mut src)?;
        assert_eq!(res, '2');
        assert_eq!(src.src.as_str(), "3");

        Ok(())
    }
}
