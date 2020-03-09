#[derive(Clone, Debug)]
enum Expr {
    Val(f64),
    Posi(Box<Expr>),
    Nega(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Pow(Box<Expr>, Box<Expr>),
}

use crate::*;
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

    fn add(e1: Self, e2: Self) -> Self {
        Add(Box::new(e1), Box::new(e2))
    }

    fn sub(e1: Self, e2: Self) -> Self {
        Sub(Box::new(e1), Box::new(e2))
    }

    fn mul(e1: Self, e2: Self) -> Self {
        Mul(Box::new(e1), Box::new(e2))
    }

    fn div(e1: Self, e2: Self) -> Self {
        Div(Box::new(e1), Box::new(e2))
    }

    fn pow(e1: Self, e2: Self) -> Self {
        Pow(Box::new(e1), Box::new(e2))
    }

    fn eval(&self) -> f64 {
        match self {
            Val(v) => *v,
            Posi(e) => e.eval(),
            Nega(e) => -e.eval(),
            Add(e1, e2) => e1.eval() + e2.eval(),
            Sub(e1, e2) => e1.eval() - e2.eval(),
            Mul(e1, e2) => e1.eval() * e2.eval(),
            Div(e1, e2) => e1.eval() / e2.eval(),
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
    (wrap('(') >> expr() << ')') | real().map(Expr::val)
}

fn expr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    mult().map2(expr_(), |e1, e2| match e2 {
        None => e1,
        Some(f) => f(e1),
    })
}

fn expr_<'a>() -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr>>> {
    fn parse_add<'a>(
        s: &mut ParseState<'a>,
        e: &mut ParseErr,
    ) -> ParseResult<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        let p = wrap('+')
            >> mult().map2(expr_(), |e1, e2| {
                Some(match e2 {
                    None => Box::new(move |e| Expr::add(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
                    Some(f) => Box::new(move |e| f(Expr::add(e, e1))),
                })
            });
        p.parse(s, e)
    }

    fn parse_sub<'a>(
        s: &mut ParseState<'a>,
        e: &mut ParseErr,
    ) -> ParseResult<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        let p = wrap('-')
            >> mult().map2(expr_(), |e1, e2| {
                Some(match e2 {
                    None => Box::new(move |e| Expr::sub(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
                    Some(f) => Box::new(move |e| f(Expr::sub(e, e1))),
                })
            });
        p.parse(s, e)
    }

    wrap(ParseFn(parse_add).attempt()) | ParseFn(parse_sub).attempt() | pure(|| None)
}

fn mult<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    uexpr().map2(mult_(), |e1, e2| match e2 {
        None => e1,
        Some(f) => f(e1),
    })
}

fn mult_<'a>(
) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr + 'static>>> {
    fn parse_mul<'a>(
        s: &mut ParseState<'a>,
        e: &mut ParseErr,
    ) -> ParseResult<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        let p = wrap('*')
            >> uexpr().map2(mult_(), |e1, e2| {
                Some(match e2 {
                    None => Box::new(move |e| Expr::mul(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
                    Some(f) => Box::new(move |e| f(Expr::mul(e, e1))),
                })
            });
        p.parse(s, e)
    }

    fn parse_div<'a>(
        s: &mut ParseState<'a>,
        e: &mut ParseErr,
    ) -> ParseResult<Option<Box<dyn FnOnce(Expr) -> Expr>>> {
        let p = wrap('/')
            >> uexpr().map2(mult_(), |e1, e2| {
                Some(match e2 {
                    None => Box::new(move |e| Expr::div(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
                    Some(f) => Box::new(move |e| f(Expr::div(e, e1))),
                })
            });
        p.parse(s, e)
    }

    wrap(ParseFn(parse_mul).attempt()) | ParseFn(parse_div).attempt() | pure(|| None)
}

fn uexpr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fn parse_posi<'a>(s: &mut ParseState<'a>, e: &mut ParseErr) -> ParseResult<Expr> {
        (wrap('+') >> uexpr().map(Expr::posi)).parse(s, e)
    }

    fn parse_nega<'a>(s: &mut ParseState<'a>, e: &mut ParseErr) -> ParseResult<Expr> {
        (wrap('-') >> uexpr().map(Expr::nega)).parse(s, e)
    }

    wrap(ParseFn(parse_posi).attempt()) | ParseFn(parse_nega).attempt() | pow()
}

fn pow<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
    fn parse_pow<'r>(s: &mut ParseState<'r>, e: &mut ParseErr) -> ParseResult<Expr> {
        (wrap(num()) << '^').map2(pow(), Expr::pow).parse(s, e)
    }

    fn parse_num<'r>(s: &mut ParseState<'r>, e: &mut ParseErr) -> ParseResult<Expr> {
        num().parse(s, e)
    }

    wrap(ParseFn(parse_pow).attempt()) | ParseFn(parse_num)
}

#[test]
fn calculator_test() {
    let mut src = ParseState::new("-1+2*3^4/5");
    let mut logger = ParseErr::default();
    let res = expr().parse(&mut src, &mut logger).value().unwrap();

    println!("{}", res.eval());
    assert!(res.eval() - 31.4 < 0.01);
    assert_eq!(src.as_str(), "");
}
