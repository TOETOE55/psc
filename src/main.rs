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

use psc::core::traits::parser::ParserExt;
use psc::covert::IntoParser;
use psc::{char, fix, pure, reg, ParseFn, ParseState, Parser};
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

fn lexem<'a, A>(
    p: impl IntoParser<ParseState<'a>, Target = A>,
) -> impl Parser<ParseState<'a>, Target = A> {
    char(' ').many_().wrap() >> p << char(' ').many_()
}

fn real<'a>() -> impl Parser<ParseState<'a>, Target = f64> {
    lexem(reg("\\d+(\\.\\d+)?"))
        .map(str::parse::<f64>)
        .map(Result::unwrap)
}

fn num<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    lexem('(').wrap() >> expr() << lexem(')') | real().map(Expr::val)
}

fn expr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    mult().map2(expr_(), |e1, e2| e2.unwrap_or(Box::new(|x| x))(e1))
}

fn expr_<'a>(
) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr + 'static>>> {
    ParseFn(|s: &mut ParseState<'a>| {
        lexem('+').parse(s)?;
        let e1 = mult().parse(s)?;
        let e2 = expr_().parse(s)?;

        Ok(Some(Box::new(move |e| match e2 {
            None => Expr::plus(e, e1),
            Some(f) => f(Expr::plus(e, e1)),
        }) as Box<dyn FnOnce(Expr) -> Expr>))
    })
    .wrap()
        | ParseFn(|s: &mut ParseState<'a>| {
            lexem(char('-')).parse(s)?;
            let e1 = mult().parse(s)?;
            let e2 = expr_().parse(s)?;
            Ok(Some(Box::new(move |e| match e2 {
                None => Expr::minu(e, e1),
                Some(f) => f(Expr::minu(e, e1)),
            }) as Box<dyn FnOnce(Expr) -> Expr>))
        })
        | pure(|| None)
}

fn mult<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    uexpr().map2(mult_(), |e1, e2| e2.unwrap_or(Box::new(|x| x))(e1))
}

fn mult_<'a>(
) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr + 'static>>> {
    ParseFn(|s: &mut ParseState<'a>| {
        lexem('*').parse(s)?;
        let e1 = uexpr().parse(s)?;
        let e2 = mult_().parse(s)?;
        Ok(Some(
            Box::new(|e| e2.unwrap_or(Box::new(|x| x))(Expr::mult(e, e1)))
                as Box<dyn FnOnce(Expr) -> Expr>,
        ))
    })
    .wrap()
        | ParseFn(|s: &mut ParseState<'a>| {
            lexem('/').parse(s)?;
            let e1 = uexpr().parse(s)?;
            let e2 = mult_().parse(s)?;
            Ok(Some(
                Box::new(|e| e2.unwrap_or(Box::new(|x| x))(Expr::divi(e, e1)))
                    as Box<dyn FnOnce(Expr) -> Expr>,
            ))
        })
        | pure(|| None)
}

fn uexpr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fix(|uexpr| {
        Box::new(
            lexem('+').wrap() >> uexpr.map(Expr::posi)
                | lexem('-').wrap() >> uexpr.map(Expr::nega)
                | pow(),
        )
    })
}

fn pow<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fix(|pow| Box::new(num().map2(lexem('^').wrap() >> pow, Expr::pow).wrap() | num()))
}

fn main() {
    let mut src = ParseState::new("- (1 +2 *(3^   4/3)- ++ (+ - - 2.9)) ?");
    let res = expr().parse(&mut src).unwrap();
    println!("{}", res.eval());
    println!("{:?}", res);
    assert_eq!(src.as_str(), "?");

    let mut src = ParseState::new("\"a\\b\"");
    let res = tok_str().parse(&mut src);
    println!("{:?}", res);

    let mut src = ParseState::new("?x1-dfas1$__2s   sdf");
    let res = tok_var().parse(&mut src);
    println!("{:?}", res);
}

fn tok_str<'a>() -> impl Parser<ParseState<'a>, Target = &'a str> {
    reg("\"([^\"]|\\.)*\"").map(|s: &str| &s[1..s.len() - 1])
}

fn tok_var<'a>() -> impl Parser<ParseState<'a>, Target = &'a str> {
    reg("\\?[0-9a-zA-Z\\-_$]+")
}
