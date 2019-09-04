pub mod core;

#[cfg(test)]
mod tests {
    use crate::core::combinators::common::{char, strg, satisfy};
    use crate::core::traits::parser::Parser;
    use crate::core::combinators::{fix, pure, Fix};
    use crate::core::err::ParseMsg;
    use crate::core::state::ParseState;
    use crate::core::traits::stream::Stream;

    fn num<S: Stream<Item=char> + Clone>() -> impl Parser<S, Target=u64> {
        satisfy(|ch: &char| ch.is_numeric())
            .some()
            .map(Vec::into_iter)
            .map(Iterator::collect::<String>)
            .map(|num| num.parse::<u64>())
            .map(Result::unwrap)
    }

    fn dynamic<S: Stream<Item=char> + Clone>() -> impl Parser<S, Target=char> {
        num().and_then(|n| if n%2 == 0 {
            Box::new(satisfy(|ch: &char| ch.is_uppercase()))
                as Box<dyn Parser<S, Target=char>>
        } else {
            Box::new(satisfy(|ch: &char| ch.is_lowercase()))
        })
    }

    fn recursion<'a, S: Stream<Item=char> + Clone>() -> impl Parser<S, Target=()> {
        fix(|this| Box::new(
            satisfy(|ch: &char| ch.is_uppercase())
                .and_r(this)
                .or(satisfy(|ch: &char| ch.is_lowercase()))))
            .and_r(pure(|| { }))
    }


    #[test]
    fn it_works() -> Result<(), ParseMsg> {
        let src = ParseState::new("1111");
        let parser = char('1').many();
        let (res, _) = parser.parse(src)?;
        assert_eq!(res, vec!['1';4]);

        let src = "1111";
        let parser = char('1').many();
        let (res, _) = parser.parse(src)?;
        assert_eq!(res, vec!['1';4]);

        let src = "11110";
        let parser = fix(Box::new(Fix::coerce(|it| Box::new(
            char('1').and_r(it).or(char('0'))))));
        let (res, _) = parser.parse(src)?;
        assert_eq!(res, '0');

        let src = ParseState::new("abcd");
        let (res, _) = char('a').and_r(strg("bcd")).parse(src)?;
        assert_eq!(res, "bcd");

        let src = "1234";
        let (res, _) = num().parse(src)?;
        assert_eq!(res, 1234);

        let src = "2H";
        let (res, _) = dynamic().parse(src)?;
        assert_eq!(res, 'H');

        let src = "OIHFa".to_string();
        let (res, _) = recursion().parse(&*src)?;
        assert_eq!(res, ());

        Ok(())
    }
}
