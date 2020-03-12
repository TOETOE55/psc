#[derive(Clone, Debug)]
pub enum Expr {
    Val(bool),
    Or(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
}

impl Expr {
    pub fn eval(&self) -> bool {
        match self {
            Expr::Val(b) => *b,
            Expr::Or(a, b) => a.eval() || b.eval(),
            Expr::And(a, b) => a.eval() && b.eval(),
        }
    }

    pub fn and(self, rhs: Self) -> Self {
        Expr::And(Box::new(self), Box::new(rhs))
    }

    pub fn or(self, rhs: Self) -> Self {
        Expr::Or(Box::new(self), Box::new(rhs))
    }

    pub fn val(v: bool) -> Self {
        Expr::Val(v)
    }
}

/**
```BCNF
expr := or
or := and '||' or | and
and := val '&&' and | val
val := '(' expr ')' | true | false
```
*/
pub mod grammar {
    use crate::tests::boolean::Expr;
    use crate::{lexeme, pure, strg, wrap, ParseFn, ParseLogger, ParseState, Parser, ParserExt};

    fn coerce_parser<P>(p: P) -> P
    where
        P: for<'a> Parser<ParseState<'a>>,
    {
        p
    }

    pub fn expr() -> impl for<'a> Parser<ParseState<'a>, Target = Expr> {
        or()
    }

    fn or() -> impl for<'a> Parser<ParseState<'a>, Target = Expr> {
        fn parse_or(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
            let v = and().parse(s, logger)?;
            lexeme("||").parse(s, logger)?;
            let a = or().parse(s, logger)?;
            Some(Expr::or(v, a))
        }

        ParseFn(parse_or).or(and())
    }

    fn and() -> impl for<'a> Parser<ParseState<'a>, Target = Expr> {
        fn parse_and(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
            let v = val().parse(s, logger)?;
            lexeme("&&").parse(s, logger)?;
            let a = and().parse(s, logger)?;
            Some(Expr::and(v, a))
        }

        ParseFn(parse_and).or(val())
    }

    fn val() -> impl for<'a> Parser<ParseState<'a>, Target = Expr> {
        fn parse_paren(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
            (wrap(lexeme('(')) >> expr() << lexeme(')')).parse(s, logger)
        }

        fn parse_bool(s: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
            let t = wrap("true") >> pure(|| Expr::val(true));
            let f = wrap("false") >> pure(|| Expr::val(false));
            t.or(f).parse(s, logger)
        }

        ParseFn(parse_paren).or(ParseFn(parse_bool))
    }
}

mod run {
    use crate::tests::boolean::grammar::expr;
    use crate::{ParseLogger, ParseState, Parser};

    #[test]
    fn boolean_test() {
        let mut src = ParseState::new("(true || false) && false");
        let mut logger = ParseLogger::default();
        let res = expr().parse(&mut src, &mut logger).unwrap();
        println!("{}", res.eval());
        assert_eq!(res.eval(), false);
        assert_eq!(src.as_str(), "");
        // let expected =
    }
}
