

#[derive(Clone, Debug)]
enum Expr {
    Val(f64),
    Posi(Box<Expr>),
    Nega(Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minu(Box<Expr>, Box<Expr>),
    Mult(Box<Expr>, Box<Expr>),
    Divi(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
}

use Expr::*;
use crate::{Parser, reg, ParserExt, wrap, pure, ParseLogger, char, lexeme};
use crate::traits::stream::ParseState;
use crate::ops::ParseFn;

impl Expr {
    fn val(v: f64) -> Self {
        Val(v)
    }

    fn posi(e: Self) -> Self {
        Posi(Box::new(e))
    }

    fn nega(e: Self) -> Self {
        Nega(Box::new(e))
    }

    fn plus(e1: Self, e2: Self) -> Self {
        Plus(Box::new(e1), Box::new(e2))
    }

    fn minu(e1: Self, e2: Self) -> Self {
        Minu(Box::new(e1), Box::new(e2))
    }

    fn mult(e1: Self, e2: Self) -> Self {
        Mult(Box::new(e1), Box::new(e2))
    }

    fn divi(e1: Self, e2: Self) -> Self {
        Divi(Box::new(e1), Box::new(e2))
    }

    fn pow(e1: Self, e2: Self) -> Self {
        Pow(Box::new(e1), Box::new(e2))
    }

    fn eval(&self) -> f64 {
        match self {
            Val(v) => *v,
            Posi(e) => e.eval(),
            Nega(e) => -e.eval(),
            Plus(e1, e2) => e1.eval() + e2.eval(),
            Minu(e1, e2) => e1.eval() - e2.eval(),
            Mult(e1, e2) => e1.eval() * e2.eval(),
            Divi(e1, e2) => e1.eval() / e2.eval(),
            Pow(e1, e2) => e1.eval().powf(e2.eval()),
        }
    }
}

fn real<'a>() -> impl Parser<ParseState<'a>, Target = f64> {
    reg("[0-9]+(/.[0-9]+)?")
        .map(str::parse::<f64>)
        .map(Result::unwrap)
}

fn num<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    wrap(char('(')) >> expr() << ')' | real().map(Expr::val)
}

fn expr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fn parse_expr(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
        let e1 = mult().parse(stream, logger)?;
        let e2 = expr_().parse(stream, logger)?;
        Some(match e2 {
            None => e1,
            Some(f) => f(e1),
        })
    }
    ParseFn(parse_expr)
}

fn expr_<'a>(
) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr>>> {
    fn parse_plus(stream: &mut ParseState, logger: &mut ParseLogger)
                  -> Option<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        lexeme('+').parse(stream, logger)?;
        let e1 = mult().parse(stream, logger)?;
        let e2 = expr_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => Box::new(move |e| Expr::plus(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
            Some(f) => Box::new(move |e| f(Expr::plus(e, e1))),
        }))
    }

    fn parse_minu(stream: &mut ParseState, logger: &mut ParseLogger)
                  -> Option<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        lexeme('-').parse(stream, logger)?;
        let e1 = mult().parse(stream, logger)?;
        let e2 = expr_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => Box::new(move |e| Expr::minu(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
            Some(f) => Box::new(move |e| f(Expr::minu(e, e1))),
        }))
    }

    wrap(ParseFn(parse_plus)) | ParseFn(parse_minu) | pure(|| None)
}

fn mult<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fn parse_mul(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
        let e1 = uexpr().parse(stream, logger)?;
        let e2 = mult_().parse(stream, logger)?;
        Some(match e2 {
            None => e1,
            Some(f) => f(e1),
        })
    }
    ParseFn(parse_mul)
}

fn mult_<'a>(
) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr>>> {
    fn parse_mul(stream: &mut ParseState, logger: &mut ParseLogger)
        -> Option<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        lexeme('*').parse(stream, logger)?;
        let e1 = uexpr().parse(stream, logger)?;
        let e2 = mult_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => Box::new(move |e| Expr::mult(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
            Some(f) => Box::new(move |e| f(Expr::mult(e, e1))),
        }))
    }

    fn parse_div(stream: &mut ParseState, logger: &mut ParseLogger)
                  -> Option<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        lexeme('/').parse(stream, logger)?;
        let e1 = uexpr().parse(stream, logger)?;
        let e2 = mult_().parse(stream, logger)?;
        Some(Some(match e2 {
            None => Box::new(move |e| Expr::divi(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
            Some(f) => Box::new(move |e| f(Expr::divi(e, e1))),
        }))
    }

    wrap(ParseFn(parse_mul)) | ParseFn(parse_div) | pure(|| None)
}

fn uexpr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fn parse_posi(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
        lexeme('+').parse(stream, logger)?;
        let e = uexpr().parse(stream, logger)?;
        Some(Expr::posi(e))
    }

    fn parse_nega(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
        lexeme('-').parse(stream, logger)?;
        let e = uexpr().parse(stream, logger)?;
        Some(Expr::nega(e))
    }

    wrap(ParseFn(parse_posi)) | ParseFn(parse_nega) | pow()
}

fn pow<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fn parse_pow(stream: &mut ParseState, logger: &mut ParseLogger) -> Option<Expr> {
        let e1 = num().parse(stream, logger)?;
        lexeme('^').parse(stream, logger)?;
        let e2 = pow().parse(stream, logger)?;
        Some(Expr::pow(e1, e2))
    }

    wrap(ParseFn(parse_pow)) | num()
}

#[test]
fn calculator_test() {
    let mut src = ParseState::new("- 1 +  2*  ++--3 ^ 4 / -5");
    let mut logger = ParseLogger::default();
    let res = expr().parse(&mut src, &mut logger).unwrap();
    println!("{}", res.eval());
    assert!(res.eval() + 33.4 < 0.01);
    assert_eq!(src.as_str(), "");
    // let expected =
}
