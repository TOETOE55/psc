use crate::core::combinators::extra::{fix, Fix, FixState};
use crate::{char, reg, ParseFn, ParseMsg, Parser};

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

fn real<'a>() -> impl Parser<FixState<'a>, Target = f64> {
    reg("[0-9]+(/.[0-9]+)?")
        .map(str::parse::<f64>)
        .map(Result::unwrap)
}

fn num<'a>() -> impl Parser<FixState<'a>, Target = Expr> {
    char('(').wrap() >> expr() << ')' | real().map(Val)
}

fn expr<'a>() -> impl Parser<FixState<'a>, Target = Expr> {
    fix(|expr| {
        Box::new(
            ParseFn(move |stream: &mut FixState<'a>| {
                let e1 = expr.parse(stream)?;
                char('+').parse(stream)?;
                let e2 = mult().parse(stream)?;
                Ok(Expr::plus(e1, e2))
            })
            .wrap()
                | ParseFn(move |stream: &mut FixState<'a>| {
                    let e1 = expr.parse(stream)?;
                    char('-').parse(stream)?;
                    let e2 = mult().parse(stream)?;
                    Ok(Expr::minu(e1, e2))
                })
                | mult(),
        )
    })
}

fn mult<'a>() -> impl Parser<FixState<'a>, Target = Expr> {
    fix(|mult| {
        Box::new(
            ParseFn(move |stream: &mut FixState<'a>| {
                let e1 = mult.parse(stream)?;
                char('*').parse(stream)?;
                let e2 = uexpr().parse(stream)?;
                Ok(Expr::mult(e1, e2))
            })
            .wrap()
                | ParseFn(move |stream: &mut FixState<'a>| {
                    let e1 = mult.parse(stream)?;
                    char('/').parse(stream)?;
                    let e2 = uexpr().parse(stream)?;
                    Ok(Expr::divi(e1, e2))
                })
                | uexpr(),
        )
    })
}

fn uexpr<'a>() -> impl Parser<FixState<'a>, Target = Expr> {
    crate::fix(|uexpr| {
        Box::new(
            (char('+').wrap() >> uexpr).map(|e| Expr::posi(e)).wrap()
                | (char('-').wrap() >> uexpr).map(|e| Expr::nega(e))
                | pow(),
        )
    })
}

fn pow<'a>() -> impl Parser<FixState<'a>, Target = Expr> {
    crate::fix(|pow| {
        Box::new(
            ParseFn(move |stream: &mut FixState<'a>| {
                let e1 = num().parse(stream)?;
                char('^').parse(stream)?;
                let e2 = pow.parse(stream)?;
                Ok(Expr::pow(e1, e2))
            })
            .wrap()
                | num(),
        )
    })
}

#[test]
fn calculator_test() {
    let mut src = FixState::new("-1+2*3^4");
    let res = expr().parse(&mut src).unwrap();
    println!("{}", res.eval());
    // assert!(res.eval() - 1.0 < 0.01);
    assert_eq!(src.as_str(), "");
    // let expected =
}
