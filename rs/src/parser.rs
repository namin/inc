//! A scheme parser in nom.
//!
//! Describes the [formal BNF
//! grammar](http://www.scheme.com/tspl2d/grammar.html) in Rust as closely as
//! possible using the nom parser combinator library.
//!
//! See
//! [lisper](https://github.com/jaseemabid/lisper/blob/master/src/Lisper/Parser.hs)
//! for a similar Haskell implementation.
//!
use super::core::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::*,
    combinator::{map, opt, value},
    multi::*,
    sequence::*,
    IResult,
};
use std::str::{self, FromStr};

pub fn program(i: &str) -> IResult<&str, AST> {
    delimited(multispace0, alt((let_syntax, datum)), multispace0)(i)
}

// (let-syntax (<syntax binding>*) <expression>+)
fn let_syntax(i: &str) -> IResult<&str, AST> {
    let (i, _) = tuple((char('('), multispace0, tag("let"), multispace1))(i)?;
    let (i, b) = delimited(char('('), many0(binding), char(')'))(i)?;
    let (i, e) = delimited(multispace0, many1(program), multispace0)(i)?;
    let (i, _) = char(')')(i)?;

    Ok((i, AST::Let { bindings: b, body: e }))
}

// named → (name value)
fn binding(i: &str) -> IResult<&str, (String, AST)> {
    let (i, _) = delimited(multispace0, char('('), multispace0)(i)?;
    let (i, (name, _, value)) = tuple((identifier, multispace0, program))(i)?;
    let (i, _) = delimited(multispace0, char(')'), multispace0)(i)?;

    Ok((i, (name, value)))
}

// Identifiers may denote variables, keywords, or symbols, depending upon
// context. They are formed from sequences of letters, digits, and special
// characters. With three exceptions, identifiers cannot begin with a
// character that can also begin a number, i.e., they cannot begin with .,
// +, -, or a digit. The three exceptions are the identifiers ..., +, and -.
// Case is insignificant in symbols so that, for example, newspaper,
// NewsPaper, and NEWSPAPER all represent the same identifier.
//
// <identifier> → <initial> <subsequent>* | + | - | ...
// <initial>    → <letter> | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
// <subsequent> → <initial> | <digit> | . | + | -
// <letter>     → a | b | ... | z
// <digit>      → 0 | 1 | ... | 9

fn identifier(i: &str) -> IResult<&str, String> {
    alt((
        value(String::from("+"), tag("+")),
        value(String::from("-"), tag("-")),
        value(String::from("..."), tag("...")),
        map(tuple((initial, many0(subsequent))), |(i, s)| {
            format!("{}{}", i, s.into_iter().collect::<String>())
        }),
    ))(i)
}

fn initial(i: &str) -> IResult<&str, char> {
    alt((letter, symbol))(i)
}

fn subsequent(i: &str) -> IResult<&str, char> {
    alt((initial, digit, one_of(".+-")))(i)
}

fn symbol(i: &str) -> IResult<&str, char> {
    one_of("!$%&*/:<=>?~_^")(i)
}

fn letter(i: &str) -> IResult<&str, char> {
    one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")(i)
}

fn digit(i: &str) -> IResult<&str, char> {
    one_of("0123456789")(i)
}

// Data include booleans, numbers, characters, strings, symbols, lists, and
// vectors. Case is insignificant in the syntax for booleans, numbers, and
// character names, but it is significant in other character constants and
// in strings. For example, #T is equivalent to #t, #E1E3 is equivalent to
// #e1e3, #X2aBc is equivalent to #x2abc, and #\NewLine is equivalent to
// #\newline; but #\A is distinct from #\a and "String" is distinct from
// string".
//
// <datum>            → <boolean> | <number> | <character> | <string> | <symbol> | <list> | <vector>
// <boolean>          → #t | #f
// <number>           → <num 2> | <num 8> | <num 10> | <num 16>
// <character>        → #\ <any character> | #\newline | #\space
// <string>           → " <string character>* "
// <string character> → \" | \\ | <any character other than" or \>
// <symbol>           →  <identifier>
// <list>             →  (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
// <abbreviation>     →  ' <datum> | ` <datum> | , <datum> | ,@ <datum>
// <vector>           → #(<datum>*)

fn datum(i: &str) -> IResult<&str, AST> {
    alt((
        (map(tag("()"), { |_| AST::Nil })),
        (map(boolean, { |b| AST::Boolean(b) })),
        (map(ascii, { |c| AST::Char(c) })),
        (map(number, { |i| AST::Number(i) })),
        (map(identifier, { |i| AST::Identifier(i) })),
        list,
    ))(i)
}

fn boolean(i: &str) -> IResult<&str, bool> {
    alt((value(true, tag("#t")), value(false, tag("#f"))))(i)
}

fn sign(i: &str) -> IResult<&str, i64> {
    alt((value(-1, tag("-")), value(1, tag("+"))))(i)
}

fn number(i: &str) -> IResult<&str, i64> {
    let (i, s) = opt(sign)(i)?;
    let (i, n) = digit1(i)?;

    // TODO: Propagate this error up rather than panic
    let n = n
        .parse::<i64>()
        .expect(&format!("Failed to parse digits into i64: `{:?}`\n", n)[..]);

    Ok((i, s.unwrap_or(1) * n))
}

// ASCII Characters for now
fn ascii(i: &str) -> IResult<&str, u8> {
    // $ man ascii
    alt((
        value(9 as u8, tag(r"#\tab")),
        value(10 as u8, tag(r"#\newline")),
        value(13 as u8, tag(r"#\return")),
        value(32 as u8, tag(r"#\space")),
        // Picking the first byte is quite unsafe, fix for UTF8
        preceded(tag(r"#\"), map(anychar, { |c: char| c as u8 })),
    ))(i)
}

// <list> → (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
fn list(i: &str) -> IResult<&str, AST> {
    let (i, _) = tuple((char('('), multispace0))(i)?;
    let (i, elems) = separated_list(multispace1, datum)(i)?;
    let (i, _) = tuple((multispace0, char(')')))(i)?;

    if elems.is_empty() {
        Ok((i, AST::Nil))
    } else {
        Ok((i, AST::List(elems)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::AST::*;

    // OK consumes all of the input and succeeds
    fn ok<T, E>(t: T) -> IResult<&'static str, T, E> {
        Ok(("", t))
    }

    // Partial consumes some of the input and succeeds
    fn partial<T, E>(rest: &str, t: T) -> IResult<&str, T, E> {
        Ok((rest, t))
    }

    // Fail denotes a parser failing without consuming any of its input
    fn fail<T>(i: &str) -> IResult<&str, T, (&str, nom::error::ErrorKind)> {
        Err(nom::Err::Error((i, nom::error::ErrorKind::Tag)))
    }

    #[test]
    fn assorted() {
        assert_eq!(ok(true), boolean("#t"));
        assert_eq!(ok(false), boolean("#f"));
        assert_eq!(fail("A"), boolean("A"));

        assert_eq!(ok('?'), symbol("?"));

        assert_eq!(ok(42), number("42"));
        assert_eq!(ok(-42), number("-42"));

        assert_eq!(ok('j' as u8), ascii("#\\j"));
        assert_eq!(ok('^' as u8), ascii("#\\^"));

        // Character parser must not consume anything unless it starts with
        // an explicit tag.
        assert_eq!(fail("test"), ascii("test"));
    }

    #[test]
    fn identifiers() {
        assert_eq!(ok(String::from("x")), identifier("x"));
        assert_eq!(ok(String::from("one")), identifier("one"));
        assert_eq!(ok(String::from("!bang")), identifier("!bang"));
        assert_eq!(ok(String::from("a->b")), identifier("a->b"));
        assert_eq!(ok(String::from("+")), identifier("+"));
        assert_eq!(ok(String::from("-")), identifier("-"));
        assert_eq!(ok(String::from("i64")), identifier("i64"));

        // -> is not an identifier, consume the - as an id and return the >
        assert_eq!(partial(">", String::from("-")), identifier("->"));

        // Identifiers must split at space and not consume anything
        // afterwards
        assert_eq!(partial(" b", String::from("a")), identifier("a b"));
    }

    // #[test]
    // fn unicode() {
    //     assert_eq!(fail(("അ")), identifier(("അ")))
    // }

    #[test]
    fn data() {
        assert_eq!(ok(Nil), datum("()"));
        assert_eq!(ok("one".into()), datum("one"));
        assert_eq!(ok(42.into()), datum("42"))
    }

    #[test]
    fn lists() {
        assert_eq!(ok(List(vec!["+".into(), 1.into()])), list("(+ 1)"));

        assert_eq!(
            ok(List(vec![
                1.into(),
                2.into(),
                3.into(),
                "a".into(),
                "b".into(),
                "c".into()
            ])),
            list("(1 2 3 a b c)")
        );

        assert_eq!(
            ok(List(vec!["inc".into(), List(vec!["inc".into(), 42.into()]),],)),
            list("(inc (inc 42))")
        );

        // Lists should throw away all spaces in between
        assert_eq!(program("(   +   1 )"), program("(+ 1)"));
    }

    #[test]
    fn binary() {
        assert_eq!(
            ok(List(vec!["+".into(), "x".into(), 1776.into()])),
            list("(+ x 1776)")
        );

        assert_eq!(
            ok(List(vec![
                "+".into(),
                "x".into(),
                List(vec!["*".into(), "a".into(), "b".into()],),
            ],)),
            list("(+ x (* a b))")
        );
    }

    #[test]
    fn top() {
        assert_eq!(ok(true.into()), program("#t"));
        assert_eq!(ok(false.into()), program("#f"));

        assert_eq!(ok('?'.into()), program("#\\?"));

        assert_eq!(ok(42.into()), program("42"));
        assert_eq!(ok((-42).into()), program("-42"));

        assert_eq!(ok('j'.into()), program("#\\j"));
        assert_eq!(ok('^'.into()), program("#\\^"));
    }

    #[test]
    fn let_binding() {
        let prog = "(let ((x 1) (y 2)) (+ x y))";

        let exp = Let {
            bindings: vec![
                ("x".to_string(), Number(1)),
                ("y".to_string(), Number(2)),
            ],
            body: vec![List(vec![
                Identifier("+".to_string()),
                Identifier("x".to_string()),
                Identifier("y".to_string()),
            ])],
        };

        assert_eq!(ok(exp), program(prog));

        assert!(program("(let ((x (let ((y 3)) (* y y)))) (cons x (+ x x)))").is_ok());
    }

    #[test]
    fn if_syntax() {
        let prog = "(if #t 12 13)";
        let exp = List(vec![
            Identifier(String::from("if")),
            Boolean(true),
            Number(12),
            Number(13),
        ]);

        assert_eq!(ok(exp), program(prog));
    }

}

/// Parse the input from user into the form the top level of the compiler
/// understands.
impl FromStr for AST {
    type Err = Error;

    fn from_str(i: &str) -> Result<Self, Error> {
        match program(i) {
            Ok((_rest, ast)) => Ok(ast),
            // Ok((parser::EMPTY, ast)) => Ok(ast),
            // Ok((_rest, _ast)) => Err(Error {
            //     message: String::from("All of input not consumed"),
            // }),
            Err(e) => Err(Error { message: format!("{:?}", e) }),
        }
    }
}
