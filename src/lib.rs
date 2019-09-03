pub mod core;


#[cfg(test)]
mod tests {
    use crate::core::combinators::common::char;
    use crate::core::traits::parser::Parser;
    use crate::core::combinators::fix;
    use std::rc::Rc;

    #[test]
    fn it_works() {
        let _parser = fix(Rc::new(|it| Box::new(char::<&str>('1').or(it))));
    }
}
