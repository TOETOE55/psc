use crate::{fix, pure, reg, satisfy, ParseFn, ParseMsg, ParseState, Parser, Stream};

mod calculator;

fn num<'a>() -> impl Parser<ParseState<'a>, Target = u64> {
    reg("[0-9]+").map(str::parse::<u64>).map(Result::unwrap)
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
            satisfy(|ch: &char| ch.is_uppercase()).wrap() >> this
                | satisfy(|ch: &char| ch.is_lowercase()),
        )
    })
    .and_r(pure(|| {}))
}

fn parse_fn<S: Stream<Item = char> + Clone>(stream: &mut S) -> Result<char, ParseMsg> {
    let parser = satisfy(|ch: &char| ch.is_uppercase()).wrap() >> ParseFn(parse_fn)
        | satisfy(|ch: &char| ch.is_lowercase());
    parser.parse(stream)
}

#[test]
fn it_works() -> Result<(), ParseMsg> {
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
    assert_eq!(res, 'a');
    assert_eq!(src.as_str(), "");

    let mut src = "123".chars();
    let parser = pure(|| vec![]).snoc('1').snoc('2').snoc('3');
    let res = parser.parse(&mut src)?;
    assert_eq!(res, vec!['1', '2', '3']);
    assert_eq!(src.as_str(), "");

    let mut src = "OIHFa".chars();
    let res = pure(|| recursion()).join().parse(&mut src)?;
    assert_eq!(res, ());
    assert_eq!(src.as_str(), "");

    Ok(())
}
