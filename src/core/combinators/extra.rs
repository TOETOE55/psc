use crate::{ParseState, Pos, Stream, Parser, ParseMsg};
use std::rc::Rc;
use crate::core::combinators::common::{Char, Strg};

#[derive(Clone, Debug)]
pub struct FixState<'a> {
    delegate: ParseState<'a>,
    depth: usize,
}

impl<'a> FixState<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            delegate: ParseState::new(src),
            depth: 0
        }
    }

    pub fn as_str(&self) -> &'a str {
        self.delegate.src.as_str()
    }

    pub fn pos(&self) -> Pos {
        self.delegate.pos
    }

    pub fn index(&self) -> usize {
        self.delegate.idx
    }

    pub fn len(&self) -> usize {
        self.delegate.len
    }

    pub fn delegate(self) -> ParseState<'a> {
        self.delegate
    }
}

impl<'a> Iterator for FixState<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        self.delegate.next()
    }
}

impl<'a> Stream for ParseState<'a> {}

/// Fixed-point Combinator
/// To deal with recursion syntax.(and left recursion)
pub struct Fix<'a, 's, A> {
    fix: Rc<dyn for<'f> Fn(&'f Self) -> Box<dyn Parser<FixState<'s>, Target = A> + 'f> + 'a>,
}

impl<'a, 's, A> Fix<'a, 's, A> {
    pub fn new<F>(fix: F) -> Self
    where
        F:  for<'f> Fn(&'f Self) -> Box<dyn Parser<FixState<'s>, Target = A> + 'f> + 'a,
    {
        Self { fix: Rc::new(fix) }
    }

    /// use to make rustc happy.
    /// # Example
    /// ```
    /// use psc::core::combinators::extra::{Fix, fix, FixState};
    /// use psc::Parser;
    /// let f = Fix::coerce(|it| Box::new(
    ///            char('1').and_r(it).or(char('0'))));
    /// let parser = fix(Box::new(f));
    ///
    /// let res = parser.parse(&mut FixState::new("1110"))?;
    /// assert_eq!(res, '0');
    /// ```
    pub fn coerce<F>(f: F) -> F
    where
        F: for<'f> Fn(&'f Self) -> Box<dyn Parser<FixState<'s>, Target = A> + 'f> + 'a,
    {
        f
    }
}

impl<'a, 's, A> Clone for Fix<'a, 's, A> {
    fn clone(&self) -> Self {
        Fix {
            fix: self.fix.clone(),
        }
    }
}

impl<'a, 's, A> Parser<FixState<'s>> for Fix<'a, 's, A> {
    type Target = A;
    fn parse(&self, stream: &mut FixState<'s>) -> Result<Self::Target, ParseMsg> {
        stream.depth += 1;
        if stream.depth + stream.delegate.idx >= stream.delegate.len() {
            return Err(ParseMsg::UnExcept("end of stream".to_string()))
        }
        (self.fix)(self).parse(stream)
    }
}

/// Create an fixed-point combinator.
/// # Example
/// ```
/// use psc::core::combinators::extra::{fix, FixState};
/// use psc::Parser;
/// let parser = fix(|it| Box::new(
///         char('1').and_r(it).or(char('0'))));
/// // parser = '1' parser | '0'
///
/// let res = parser.parse(&mut FixState::new("1110"))?;
/// assert_eq!(res, '0');
/// ```
pub fn fix<'a, 's, A, F>(fix: F) -> Fix<'a, 's, A>
where
    F: for<'f> Fn(&'f Fix<'a, 's, A>) -> Box<dyn Parser<FixState<'s>, Target = A> + 'f> + 'a,
{
    Fix::new(fix)
}

impl<'a> Parser<FixState<'a>> for Char<FixState<'a>> {
    type Target = char;
    fn parse(&self, stream: &mut FixState<'a>) -> Result<Self::Target, ParseMsg> {
        let ch = stream.next().ok_or(ParseMsg::EOF)?;
        if self.ch == ch {
            Ok(ch)
        } else {
            Err(ParseMsg::Except(format!(
                "expected {}, found {} at {:?}.",
                self.ch, ch, stream.pos()
            )))
        }
    }
}

impl<'a, 's> Parser<FixState<'s>> for Strg<'a, FixState<'s>> {
    type Target = &'s str;
    fn parse(&self, stream: &mut FixState<'s>) -> Result<Self::Target, ParseMsg> {
        let re = regex::Regex::new(self.s).unwrap();
        let src = stream.as_str();
        match re.find(src) {
            Some(range) if range.start() == 0 => {
                let (matched, rest) = src.split_at(range.end());
                stream.delegate.src = rest.chars();
                Ok(matched)
            },
            _ => Err(ParseMsg::Except(format!(
                "expected {} at {:?}",
                self.s, stream.pos()
            ))),
        }
    }
}


#[cfg(test)]
mod test {
    use crate::extra::{fix, FixState};
    use crate::{Parser, char};

    #[test]
    fn left_rec() {
        let parser = fix(|it| Box::new(
            it.wrap() >> char('1') | char('0')
        ));
        // parser = parser  '1' | '0'
        let mut src = FixState::new("01112");
        let res = parser.parse(&mut src).unwrap();
        assert_eq!(res, '1');
        assert_eq!(src.as_str(), "2");
    }
}