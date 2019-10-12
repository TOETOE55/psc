pub mod core;

pub use crate::core::{
    combinators::{
        ops::{ParseFn, ParserWrapper},
        common::{char, satisfy, strg},
        extra,
        eof, failure, fix, pure,
    },
    err::ParseMsg,
    state::{ParseState, Pos},
    traits::{parser::Parser, stream::Stream},
};

#[cfg(test)]
mod tests {

    use crate::{ParseFn, Stream, satisfy, Parser, pure, fix, ParseMsg, ParseState, strg, char};
    use crate::core::combinators::Fix;

    fn num<S: Stream<Item = char> + Clone>() -> impl Parser<S, Target = u64> {
        satisfy(|ch: &char| ch.is_numeric())
            .some()
            .map(Vec::into_iter)
            .map(Iterator::collect::<String>)
            .map(|num| num.parse::<u64>())
            .map(Result::unwrap)
    }

    fn dynamic<'a, S: Stream<Item = char> + Clone + 'a>() -> impl Parser<S, Target = char> + 'a {
        num().and_then(|n| {
            if n % 2 == 0 {
                Box::new(satisfy(|ch: &char| ch.is_uppercase()))
                    as Box<dyn Parser<S, Target = char>>
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
        let mut src = ParseState::new("1111");
        let parser = char('1').many();
        let res = parser.parse(&mut src)?;
        assert_eq!(res, vec!['1'; 4]);
        assert_eq!(src.src.as_str(), "");

        let mut src = "1111".chars();
        let parser = char('1').many();
        let res = parser.parse(&mut src)?;
        assert_eq!(res, vec!['1'; 4]);
        assert_eq!(src.as_str(), "");

        let mut src = "11110".chars();
        let parser = fix(Box::new(Fix::coerce(|it| {
            Box::new(char('1').and_r(it).or(char('0')))
        })));
        let res = parser.parse(&mut src)?;
        assert_eq!(res, '0');
        assert_eq!(src.as_str(), "");

        let mut src = ParseState::new("abcd");
        let res = char('a').and_r(strg("bcd")).parse(&mut src)?;
        assert_eq!(res, "bcd");
        assert_eq!(src.src.as_str(), "");

        let mut src = "1234".chars();
        let res = num().parse(&mut src)?;
        assert_eq!(res, 1234);
        assert_eq!(src.as_str(), "");

        let mut src = "2H".chars();
        let res = dynamic().parse(&mut src)?;
        assert_eq!(res, 'H');
        assert_eq!(src.as_str(), "");

        let mut src = "OIHFa".chars();
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
