//! A scheme parser in nom.
//!
//! Describes the [formal BNF grammar][grammar] in Rust as closely as possible
//! using the nom parser combinator library.
//!
//! ✏ This module is heavily documented and the order of declaration follow the
//! grammar; so the source best read sequentially in the declared order rather
//! than alphabetically here.
//!
//! See [lisper][lisper] for a similar Haskell implementation.
//!
//! [grammar]: http://www.scheme.com/tspl2d/grammar.html
//! [lisper]: https://github.com/jaseemabid/lisper/blob/master/src/Lisper/Parser.hs
use super::core::*;
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag},
    character::complete::{multispace0 as space0, multispace1 as space1, *},
    combinator::{map, opt, value},
    multi::*,
    sequence::*,
    IResult,
};
use std::str;

/// A program consists of a sequence of definitions and expressions.
///
/// ```BNF
/// <program>  → <form>*
/// <form>     → <definition> | <expression>
/// ```
pub fn program(i: &str) -> IResult<&str, Vec<AST>> {
    many1(form)(i)
}

pub fn form(i: &str) -> IResult<&str, AST> {
    alt((definition, expression))(i)
}

/// Definitions include various forms of declarations
///
/// Definitions include variable and syntax definitions, begin forms containing
/// zero or more definitions, let-syntax and letrec-syntax forms expanding into
/// zero or more definitions, and derived definitions. Derived definitions are
/// syntactic extensions that expand into some form of definition. A transformer
/// expression is a syntax-rules form or some other expression that produces a
/// transformer.
///
/// ```BNF
///
/// <definition> → <variable definition>
///              | <syntax definition>
///              | (begin <definition>*)
///              | (let-syntax (<syntax binding>*) <definition>*)
///              | (letrec-syntax (<syntax binding>*) <definition>*)
///              | <derived definition>
///
/// <variable definition> → (define <variable> <expression>)
///                       | (define (<variable> <variable>*) <body>)
///                       | (define (<variable> <variable>* . <variable>) <body>)
///
/// <variable>          → <identifier>
/// <body>              → <definition>* <expression>+
/// <syntax definition> → (define-syntax <keyword> <transformer expression>)
/// <keyword>           → <identifier>
/// <syntax binding>    → (<keyword> <transformer expression>)
/// ```
fn definition(i: &str) -> IResult<&str, AST> {
    alt((let_syntax, if_syntax))(i)
}

/// `(let-syntax (<syntax binding>*) <expression>+)`
fn let_syntax(i: &str) -> IResult<&str, AST> {
    let (i, _) = tuple((open, tag("let"), space1))(i)?;
    let (i, bindings) = delimited(open, many0(binding), close)(i)?;
    let (i, body) =
        delimited(space0, many1(terminated(expression, space0)), space0)(i)?;
    let (i, _) = close(i)?;

    Ok((i, AST::Let { bindings, body }))
}

/// `named → (name value)`
fn binding(i: &str) -> IResult<&str, (String, AST)> {
    let (i, (_, name, _, value, _, _)) =
        tuple((open, identifier, space1, expression, close, space0))(i)?;

    Ok((i, (name, value)))
}

/// Core expressions
///
/// Expressions include core expressions, let-syntax or letrec-syntax forms
/// expanding into a sequence of one or more expressions, and derived
/// expressions. The core expressions are self-evaluating constants, variable
/// references, applications, and quote, lambda, if, and set! expressions.
/// Derived expressions include and, begin, case, cond, delay, do, let, let*,
/// letrec, or, and quasiquote expressions plus syntactic extensions that expand
/// into some form of expression.
///
/// ```BNF
/// <expression>  → <constant>
///               | <variable>
///               | (quote <datum>) | ' <datum>
///               | (lambda <formals> <body>)
///               | (if <expression> <expression> <expression>)
///               | (if <expression> <expression>)
///               | (set! <variable> <expression>)
///               | <application>
///               | (let-syntax (<syntax binding>*) <expression>+)
///               | (letrec-syntax (<syntax binding>*) <expression>+)
///               | <derived expression>
///
/// <constant>    → <boolean> | <number> | <character> | <string>
/// <formals>     → <variable> | (<variable>*) | (<variable>+ . <variable>)
/// <application> → (<expression> <expression>*)
/// ```
fn expression(i: &str) -> IResult<&str, AST> {
    alt((constant, variable, lambda_syntax, if_syntax, let_syntax, application))(
        i,
    )
}

/// `(lambda <formals> <body>)`
fn lambda_syntax(i: &str) -> IResult<&str, AST> {
    let (i, (_, _, _, args, _, body, _)) =
        tuple((open, tag("lambda"), space1, formals, space0, body, close))(i)?;

    Ok((i, AST::Lambda { args, body }))
}

/// `(if <expression> <expression> <expression>) | (if <expression> <expression>)`
fn if_syntax(i: &str) -> IResult<&str, AST> {
    let (i, (_, _, _, pred, _, then, _, alt, _)) = tuple((
        open,
        tag("if"),
        space1,
        expression,
        space1,
        expression,
        space0,
        opt(expression),
        close,
    ))(i)?;

    let pred = Box::new(pred);
    let then = Box::new(then);
    let alt = alt.map(Box::new);

    Ok((i, AST::Cond { pred, then, alt }))
}

/// variable is an identifier
fn variable(i: &str) -> IResult<&str, AST> {
    map(identifier, { |i| AST::Identifier(i) })(i)
}

/// `<formals>     → <variable> | (<variable>*) | (<variable>+ . <variable>)`
fn formals(i: &str) -> IResult<&str, Vec<String>> {
    alt((
        map(identifier, |s| vec![s]),
        delimited(open, many0(terminated(identifier, space0)), close),
    ))(i)
}

/// `<body> → <definition>* <expression>+`
fn body(i: &str) -> IResult<&str, Vec<AST>> {
    let (i, mut ds) = many0(definition)(i)?;
    let (i, mut es) = many1(expression)(i)?;

    let mut v = Vec::new();
    v.append(&mut ds);
    v.append(&mut es);

    Ok((i, v))
}

/// `<constant> → <boolean> | <number> | <character> | <string>`
fn constant(i: &str) -> IResult<&str, AST> {
    alt((
        (map(tag("()"), { |_| AST::Nil })),
        (map(ascii, { |c| AST::Char(c) })),
        (map(boolean, { |b| AST::Boolean(b) })),
        (map(number, { |i| AST::Number(i) })),
        (map(string, { |i| AST::Str(i) })),
    ))(i)
}

/// `<application> → (<expression> <expression>*)`
fn application(i: &str) -> IResult<&str, AST> {
    let (i, (_, a, _, mut b, _)) = tuple((
        open,
        expression,
        space1,
        many1(terminated(expression, space0)),
        close,
    ))(i)?;

    let mut v = vec![a];
    v.append(&mut b);

    Ok((i, AST::List(v)))
}

/// Identifiers may denote variables, keywords, or symbols depending upon
/// context.
///
/// They are formed from sequences of letters, digits, and special
/// characters. With three exceptions, identifiers cannot begin with a
/// character that can also begin a number, i.e., they cannot begin with .,
/// +, -, or a digit. The three exceptions are the identifiers ..., +, and -.
/// Case is insignificant in symbols so that, for example, newspaper,
/// NewsPaper, and NEWSPAPER all represent the same identifier.
///
/// ```BNF
/// <identifier> → <initial> <subsequent>* | + | - | ...
/// <initial>    → <letter> | ! | $ | % | & | * | / | : | < | = | > | ? | ~ | _ | ^
/// <subsequent> → <initial> | <digit> | . | + | -
/// <letter>     → a | b | ... | z
/// <digit>      → 0 | 1 | ... | 9
/// ```
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

/// Data include booleans, numbers, characters, strings, symbols, lists, and
/// vectors.
///
/// Case is insignificant in the syntax for booleans, numbers, and
/// character names, but it is significant in other character constants and
/// in strings. For example, #T is equivalent to #t, #E1E3 is equivalent to
/// #e1e3, #X2aBc is equivalent to #x2abc, and #\NewLine is equivalent to
/// #\newline; but #\A is distinct from #\a and "String" is distinct from
/// string".
///
/// ```BNF
/// <datum>            → <boolean> | <number> | <character> | <string> | <symbol> | <list> | <vector>
/// <boolean>          → #t | #f
/// <number>           → <num 2> | <num 8> | <num 10> | <num 16>
/// <character>        → #\ <any character> | #\newline | #\space
/// <string>           → " <string character>* "
/// <string character> → \" | \\ | <any character other than" or \>
/// <symbol>           →  <identifier>
/// <list>             →  (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
/// <abbreviation>     →  ' <datum> | ` <datum> | , <datum> | ,@ <datum>
/// <vector>           → #(<datum>*)
/// ```
#[cfg(test)]
fn datum(i: &str) -> IResult<&str, AST> {
    alt((
        (map(tag("()"), { |_| AST::Nil })),
        (map(boolean, { |b| AST::Boolean(b) })),
        (map(ascii, { |c| AST::Char(c) })),
        (map(number, { |i| AST::Number(i) })),
        (map(identifier, { |i| AST::Identifier(i) })),
        (map(string, { |i| AST::Str(i) })),
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

/// ASCII Characters for now
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

fn string(i: &str) -> IResult<&str, String> {
    let q = "\"";
    let (i, s) = delimited(tag(q), opt(is_not(q)), tag(q))(i)?;

    Ok((i, s.map_or(String::from(""), |s| s.to_string())))
}

/// `<list> → (<datum>*) | (<datum>+ . <datum>) | <abbreviation>`
#[cfg(test)]
fn list(i: &str) -> IResult<&str, AST> {
    let (i, _) = tuple((char('('), space0))(i)?;
    let (i, elems) = separated_list(space1, datum)(i)?;
    let (i, _) = tuple((space0, char(')')))(i)?;

    if elems.is_empty() {
        Ok((i, AST::Nil))
    } else {
        Ok((i, AST::List(elems)))
    }
}

fn open(i: &str) -> IResult<&str, ()> {
    let (i, _) = tuple((char('('), space0))(i)?;
    Ok((i, ()))
}

fn close(i: &str) -> IResult<&str, ()> {
    let (i, _) = tuple((space0, char(')')))(i)?;
    Ok((i, ()))
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

        assert_eq!(ok(b'j'), ascii("#\\j"));
        assert_eq!(ok(b'^'), ascii("#\\^"));

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
        assert_eq!(ok(42.into()), datum("42"));
    }

    #[test]
    fn strings() {
        assert_eq!(ok(Str("hello world".into())), datum("\"hello world\""));
        assert_eq!(
            ok(Str("മലയാളം".into())),
            datum("\"മലയാളം\"")
        );

        assert_eq!(
            ok(Str("Unicode 😱 ⌘".into())),
            datum("\"Unicode 😱 ⌘\"")
        );

        assert_eq!(ok(Str("".into())), datum("\"\""));
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
        assert_eq!(ok(vec![true.into()]), program("#t"));
        assert_eq!(ok(vec![false.into()]), program("#f"));

        assert_eq!(ok(vec!['?'.into()]), program("#\\?"));

        assert_eq!(ok(vec![42.into()]), program("42"));
        assert_eq!(ok(vec![(-42).into()]), program("-42"));

        assert_eq!(ok(vec!['j'.into()]), program("#\\j"));
        assert_eq!(ok(vec!['^'.into()]), program("#\\^"));
    }

    #[test]
    fn let_syntax() {
        let p1 = "(let ((x 1) (y 2)) (+ x y))";
        let p2 = "(let ((x 1)) (let ((x 2)) #t) x)";

        let e1 = Let {
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

        let e2 = Let {
            bindings: vec![("x".to_string(), Number(1))],
            body: vec![
                Let {
                    bindings: vec![("x".to_string(), Number(2))],
                    body: vec![true.into()],
                },
                Identifier("x".to_string()),
            ],
        };

        assert_eq!(ok(e1), super::let_syntax(p1));
        assert_eq!(ok(e2), super::let_syntax(p2));

        assert!(program(
            "(let ((x (let ((y (+ 1 2))) (* y y)))) (cons x (+ x x)))"
        )
        .is_ok());

        assert!(program("(let ((x (let ((y 3)) (* y y)))) (cons x (+ x x)))")
            .is_ok());
    }

    #[test]
    fn if_syntax() {
        let prog = "(if #t 12 13)";
        let exp = Cond {
            pred: Box::new(Boolean(true)),
            then: Box::new(Number(12)),
            alt: Some(Box::new(Number(13))),
        };

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(if #t 14)";
        let exp = Cond {
            pred: Box::new(Boolean(true)),
            then: Box::new(Number(14)),
            alt: None,
        };

        assert_eq!(ok(vec![exp]), program(prog));
    }

    #[test]
    fn lambda_syntax() {
        let prog = "(lambda () 1)";
        let exp = Lambda { args: vec![], body: vec![Number(1)] };

        assert_eq!(ok(vec![exp]), program(prog));

        let prog = "(lambda (a b ) a)";
        let exp = Lambda {
            args: vec!["a".into(), "b".into()],
            body: vec![Identifier("a".into())],
        };

        assert_eq!(ok(vec![exp]), program(prog));
        // assert_eq!(ok(exp), super::lambda_syntax(prog));

        let prog = "(lambda (a b) (+ b a))";
        let exp = Lambda {
            args: vec!["a".into(), "b".into()],
            body: vec![AST::List(vec!["+".into(), "b".into(), "a".into()])],
        };

        assert_eq!(ok(vec![exp]), program(prog));
        // assert_eq!(ok(exp), super::lambda_syntax(prog));

        let prog = "(lambda a a)";
        let exp = Lambda {
            args: vec!["a".into()],
            body: vec![Identifier("a".into())],
        };

        assert_eq!(ok(vec![exp]), program(prog));
    }
}

/// Parse the input from user into the form the top level of the compiler
/// understands.
pub fn parse(i: &str) -> Result<Expressions, Error> {
    match program(i) {
        Ok((_rest, expressions)) => Ok(Expressions { 0: expressions }),
        // Ok((EMPTY, ast)) => Ok(ast),
        // Ok((_rest, _ast)) => Err(Error {
        //     message: String::from("All of input not consumed"),
        // }),
        Err(e) => Err(Error { message: format!("{:?}", e) }),
    }
}
