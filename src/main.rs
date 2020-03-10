//#[derive(Clone, Debug)]
//enum Expr {
//    Val(f64),
//    Posi(Box<Expr>),
//    Nega(Box<Expr>),
//    Plus(Box<Expr>, Box<Expr>),
//    Minu(Box<Expr>, Box<Expr>),
//    Mult(Box<Expr>, Box<Expr>),
//    Divi(Box<Expr>, Box<Expr>),
//    Pow(Box<Expr>, Box<Expr>),
//}
//
//use psc::{char, pure, reg, ParseFn, ParseState, Parser};
//use Expr::*;
//
//impl Expr {
//    fn val(v: f64) -> Self {
//        Val(v)
//    }
//
//    fn posi(e: Self) -> Self {
//        Posi(Box::new(e))
//    }
//
//    fn nega(e: Self) -> Self {
//        Nega(Box::new(e))
//    }
//
//    fn plus(e1: Self, e2: Self) -> Self {
//        Plus(Box::new(e1), Box::new(e2))
//    }
//
//    fn minu(e1: Self, e2: Self) -> Self {
//        Minu(Box::new(e1), Box::new(e2))
//    }
//
//    fn mult(e1: Self, e2: Self) -> Self {
//        Mult(Box::new(e1), Box::new(e2))
//    }
//
//    fn divi(e1: Self, e2: Self) -> Self {
//        Divi(Box::new(e1), Box::new(e2))
//    }
//
//    fn pow(e1: Self, e2: Self) -> Self {
//        Pow(Box::new(e1), Box::new(e2))
//    }
//
//    fn eval(&self) -> f64 {
//        match self {
//            Val(v) => *v,
//            Posi(e) => e.eval(),
//            Nega(e) => -e.eval(),
//            Plus(e1, e2) => e1.eval() + e2.eval(),
//            Minu(e1, e2) => e1.eval() - e2.eval(),
//            Mult(e1, e2) => e1.eval() * e2.eval(),
//            Divi(e1, e2) => e1.eval() / e2.eval(),
//            Pow(e1, e2) => e1.eval().powf(e2.eval()),
//        }
//    }
//}
//
//fn real<'a>() -> impl Parser<ParseState<'a>, Target = f64> {
//    reg("[0-9]+(/.[0-9]+)?")
//        .map(str::parse::<f64>)
//        .map(Result::unwrap)
//}
//
//fn num<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
//    char('(').wrap() >> expr() << ')' | real().map(Val)
//}
//
//fn expr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
//    ParseFn(|s: &mut ParseState<'a>| {
//        let e1 = mult().parse(s)?;
//        let e2 = expr_().parse(s)?;
//        match e2 {
//            None => Ok(e1),
//            Some(f) => Ok(f(e1)),
//        }
//    })
//}
//
//fn expr_<'a>(
//) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr + 'static>>> {
//    ParseFn(|s: &mut ParseState<'a>| {
//        char('+').parse(s)?;
//        let e1 = mult().parse(s)?;
//        let e2 = expr_().parse(s)?;
//        Ok(Some(match e2 {
//            None => Box::new(move |e| Expr::plus(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
//            Some(f) => Box::new(move |e| f(Expr::plus(e, e1))),
//        }))
//    })
//    .wrap()
//        | ParseFn(|s: &mut ParseState<'a>| {
//            char('-').parse(s)?;
//            let e1 = mult().parse(s)?;
//            let e2 = expr_().parse(s)?;
//            Ok(Some(match e2 {
//                None => Box::new(move |e| Expr::minu(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
//                Some(f) => Box::new(move |e| f(Expr::minu(e, e1))),
//            }))
//        })
//        | pure(|| None)
//}
//
//fn mult<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
//    ParseFn(|s: &mut ParseState<'a>| {
//        let e1 = uexpr().parse(s)?;
//        let e2 = mult_().parse(s)?;
//        match e2 {
//            None => Ok(e1),
//            Some(f) => Ok(f(e1)),
//        }
//    })
//}
//
//fn mult_<'a>(
//) -> impl Parser<ParseState<'a>, Target = Option<Box<dyn FnOnce(Expr) -> Expr + 'static>>> {
//    ParseFn(|s: &mut ParseState<'a>| {
//        char('*').parse(s)?;
//        let e1 = uexpr().parse(s)?;
//        let e2 = mult_().parse(s)?;
//        Ok(Some(match e2 {
//            None => Box::new(move |e| Expr::mult(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
//            Some(f) => Box::new(move |e| f(Expr::plus(e, e1))),
//        }))
//    })
//    .wrap()
//        | ParseFn(|s: &mut ParseState<'a>| {
//            char('/').parse(s)?;
//            let e1 = uexpr().parse(s)?;
//            let e2 = mult_().parse(s)?;
//            Ok(Some(match e2 {
//                None => Box::new(move |e| Expr::divi(e, e1)) as Box<dyn FnOnce(Expr) -> Expr>,
//                Some(f) => Box::new(move |e| f(Expr::minu(e, e1))),
//            }))
//        })
//        | pure(|| None)
//}
//
//fn uexpr<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
//    ParseFn(|s: &mut ParseState<'a>| {
//        char('+').parse(s)?;
//        let e = uexpr().parse(s)?;
//        Ok(Expr::posi(e))
//    })
//    .wrap()
//        | ParseFn(|s: &mut ParseState<'a>| {
//            char('-').parse(s)?;
//            let e = uexpr().parse(s)?;
//            Ok(Expr::nega(e))
//        })
//        | pow()
//}
//
//fn pow<'a>() -> impl Parser<ParseState<'a>, Target = Expr> {
//    ParseFn(move |stream: &mut ParseState<'a>| {
//        let e1 = num().parse(stream)?;
//        char('^').parse(stream)?;
//        let e2 = pow().parse(stream)?;
//        Ok(Expr::pow(e1, e2))
//    })
//    .wrap()
//        | num()
//}

fn main() {
    //    let mut src = ParseState::new("-1+2*3^4");
    //    let res = expr().parse(&mut src).unwrap();
    //    println!("{}", res.eval());
    //    println!("{:?}", res);
    //    assert!(res.eval() - 161.0 < 0.01);
    //    assert_eq!(src.as_str(), "");
}
