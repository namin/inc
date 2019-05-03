use nom::types::CompleteByteSlice as S;
use std::fs::File;
use std::io::Write;
use std::str::FromStr;

// Control behavior and external interaction of the program.
pub struct Config {
    // Program is the input source
    pub program: String,
    // Outfile is the name of the generated asm and executable
    pub output: String,
}

impl Config {
    pub fn asm(&self) -> String {
        let stdout = String::from("/dev/stdout");
        if self.output == stdout {
            stdout
        } else {
            format!("{}.s", self.output)
        }
    }
}

// Custom error type for all of inc. This might not be idiomatic Rust, revisit
// later.
#[derive(Debug)]
pub struct Error {
    message: String,
}

// The LISP AST
#[derive(Debug, PartialEq)]
pub enum AST {
    Nil,
    Number { i: i64 },
    Boolean { b: bool },
    // A unicode char encoded in UTF-8 can take upto 4 bytes and won't fit in a
    // word; so this implementation makes sense only for ASCII.
    Char { c: u8 },
    Identifier { i: String },
    // Since Rust needs to know the size of the AST type upfront, we need an
    // indirection here with Box for recursive types. The same applies again for
    // the nested contents, so we use a Vec instead of an `[AST]`.
    List {l: Box<Vec<AST>>}
}

impl AST {
    pub fn t() -> AST {
        AST::Boolean { b: true }
    }

    pub fn f() -> AST {
        AST::Boolean { b: false }
    }

    pub fn id(i: &str ) -> AST {
        AST::Identifier {i: String::from(i)}
    }

    pub fn num(i: i64 ) -> AST {
        AST::Number {i}
    }
}

// The scheme parser
//
// See http://www.scheme.com/tspl2d/grammar.html for formal grammar
// Ported from https://github.com/jaseemabid/lisper/blob/master/src/Lisper/Parser.hs
//
mod parser {

    use super::*;
    use nom::types::CompleteByteSlice as S;
    use nom::{self, *};
    use std::str;

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
    //
    named!(identifier <S , String>, alt!(
        value!(String::from("+"), tag!("+"))
      | value!(String::from("-"), tag!("-"))
      | value!(String::from("..."), tag!("..."))
      | do_parse!(
          i: initial >>
          s: many1!(subsequent) >>
          (format!("{}{}", i, s.into_iter().collect::<String>())))
    ));

    named!(initial <S, char>, alt!(letter | symbol));

    named!(subsequent <S, char>, alt!(initial | digit | one_of!(".+-")));

    named!(symbol <S, char>, one_of!("!$%&*/:<=>?~_^"));

    named!(letter <S, char>, one_of!("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"));

    named!(digit <S, char>, one_of!("0123456789"));

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

    named!(sign <S, i64>, alt!(
        tag!("-") => { |_| -1 } |
        tag!("+") => { |_|  1 }));

    named!(boolean <S, bool>, alt!(
        tag!("#t") => { |_| true } |
        tag!("#f") => { |_| false }));

    // ASCII Characters for now
    named!(ascii <S, u8>, alt!(

        // $ man ascii
        value!(9  as u8, tag!(r"#\tab")) |
        value!(10 as u8, tag!(r"#\newline")) |
        value!(13 as u8, tag!(r"#\return")) |
        value!(32 as u8, tag!(r"#\space")) |

        // Picking the first byte is quite unsafe, fix for UTF8
        preceded!(tag!(r"#\"), map!(take!(1), { |e: S| e.0[0] }))
    ));

    // This isn't quite right
    named!(number <S, i64>, do_parse!(
        s: opt!(sign) >>
        n: map!(take_while1!(is_digit),
                { |e: S| str::from_utf8(e.0)
                   .expect("Failed to parse string into UTF-8")
                   .parse::<i64>()
                   .expect(&format!("Failed to parse digits into i64: `{:?}`\n", e.0)[..])
                }) >>
            (s.unwrap_or(1) * n)
    ));

    named!(datum <S, AST>, alt!(
        value!(AST::Nil, tag!("()"))            |
        boolean    => { |b| AST::Boolean{b} }   |
        ascii      => { |c| AST::Char{c} }      |
        number     => { |i| AST::Number{i} }    |
        identifier => { |i| AST::Identifier{i} }
    ));

    // <list> → (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
    named!(pub list <S, AST>, do_parse!(
        char!('(') >>
        opt!(many0!(space)) >>
        d: map!(separated_list!(space, datum),
                {|ls: Vec<AST> |
                 if ls.is_empty() {
                     AST::Nil
                 } else {
                     AST::List{l: Box::new(ls)}
                 }}) >>
        opt!(many0!(space)) >>
        char!(')') >>
        (d)));

    #[cfg(test)]
    mod test {
        use super::*;

        // The complete input is parsed and there is nothing left.
        const EMPTY: S<'static> = S(b"");

        // OK consumes all of the input and succeeds
        fn ok<T>(t: T) -> Result<(S<'static>, T), nom::Err<S<'static>, u32>> {
            partial(EMPTY, t)
        }

        // Partial consumes some of the input and succeeds
        fn partial<T>(unconsumed: S<'static>, t: T) -> Result<(S<'_>, T), nom::Err<S<'_>, u32>> {
            Ok((unconsumed, t))
        }

        // Fail denotes a parser failing without consuming any of its input
        fn fail<T>(unconsumed: S<'_>) -> Result<(S<'_>, T), nom::Err<S, u32>> {
            Err(Err::Error(Context::Code(unconsumed, ErrorKind::Alt)))
        }

        #[test]
        fn assorted() {
            assert_eq!(ok(true), boolean(S(b"#t")));
            assert_eq!(ok(false), boolean(S(b"#f")));
            assert_eq!(fail(S(b"A")), boolean(S(b"A")));

            assert_eq!(ok('?'), symbol(S(b"?")));

            assert_eq!(ok(42), number(S(b"42")));
            assert_eq!(ok(-42), number(S(b"-42")));

            assert_eq!(ok('j' as u8), ascii(S(b"#\\j")));
            assert_eq!(ok('^' as u8), ascii(S(b"#\\^")));
        }

        #[test]
        fn identifiers() {
            assert_eq!(ok(String::from("one")), identifier(S(b"one")));
            assert_eq!(ok(String::from("!bang")), identifier(S(b"!bang")));
            assert_eq!(ok(String::from("a->b")), identifier(S(b"a->b")));
            assert_eq!(ok(String::from("+")), identifier(S(b"+")));
            assert_eq!(ok(String::from("-")), identifier(S(b"-")));
            assert_eq!(ok(String::from("i64")), identifier(S(b"i64")));

            // -> is not an identifier, consume the - as an id and return the >
            assert_eq!(partial(S(b">"), String::from("-")), identifier(S(b"->")));
        }

        // #[test]
        // fn unicode() {
        //     assert_eq!(fail(S(b"അ")), identifier(S(b"അ")))
        // }

        #[test]
        fn data() {
            assert_eq!(ok(AST::Nil), datum(S(b"()")));
            assert_eq!(ok(AST::Identifier{i: String::from("one")}), datum(S(b"one")));
            assert_eq!(ok(AST::Number{i: 42}), datum(S(b"42")))
        }

        #[test]
        fn oneplus() {
            let p = AST::List{l: Box::new(vec![AST::id("+"), AST::Number{i: 1}])};
            assert_eq!(ok(p), list(S(b"(+ 1)")));

            let q = AST::List{l: Box::new(vec![AST::id("+"), AST::Number{i: 1}])};
            assert_eq!(ok(q), list(S(b"(  +   1 )")))
        }
    }
}

// Parse the input from user into the form the top level of the compiler
// understands.
impl FromStr for AST {
    type Err = Error;

    fn from_str(program: &str) -> Result<Self, Error> {
        match parser::list(S(program.as_bytes())) {
            Ok((_rest, ast)) => Ok(ast),
            // Ok((parser::EMPTY, ast)) => Ok(ast),
            // Ok((_rest, _ast)) => Err(Error {
            //     message: String::from("All of input not consumed"),
            // }),
            Err(e) => Err(Error {
                message: format!("{}", e),
            }),
        }
    }
}

pub fn compile(config: &mut Config) -> Result<(), Error> {
    let i: AST = config.program.parse::<AST>()?;

    let mut handler = File::create(&config.asm())
        .unwrap_or_else(|_| panic!("Failed to create {}", &config.asm()));

    match handler.write_all(emit::program(i).as_bytes()) {
        Ok(_) => Ok(()),
        Err(e) => Err(Error {
            message: format!("Failed to write generated code: {}", e),
        }),
    }
}

#[cfg(target_os = "macos")]
mod emit {
    use super::immediate;
    use super::*;

    fn label(label: &str) -> String {
        format!("{}:\n", label)
    }

    fn function_header(name: &str) -> String {
        let mut ctx = String::new();

        ctx.push_str("  .section __TEXT,__text\n");
        ctx.push_str(&format!("  .globl {}\n", &name));
        ctx.push_str(&label(&name));
        ctx
    }

    // Move the argument to RAX
    fn rax(word: i64) -> String {
        format!("  movq ${}, %rax\n", word)
    }

    // Add `k` to register RAX
    fn add(k: i64) -> String {
        format!("  add ${}, %rax\n", immediate::to(&AST::num(k)))
    }

    // eval a program and move result to RAX
    fn eval(prog:AST) -> String {
        match prog {
            AST::List { l } => match l.as_slice() {
                [AST::Identifier{i}, arg] => match &i[..] {
                    "inc" => {
                        let mut ctx = String::new();

                        // mov `prog` RAX
                        ctx.push_str(&rax(immediate::to(&arg)));

                        // add 1 rax
                        ctx.push_str(&add(1));
                        ctx
                    }
                    _ => unimplemented!()
                },
                _ => unimplemented!()
            },
            _ => rax(immediate::to(&prog))
        }
    }

    pub fn program(prog: AST) -> String {
        let mut ctx = String::new();

        ctx.push_str(&function_header("_init")[..]);
        ctx.push_str(&eval(prog));
        ctx.push_str("  retq\n");
        ctx
    }
}

// Constants for runtime representation
//
// Immediate values (values that can be fit in one machine word) are tagged for
// distinguising them from heap allocated pointers. The last 3 bits effectively
// serve as the runtime type of the value. Always using 3 bits is a simpler
// approach than the multi bit technique the paper uses. This is a very
// efficient and low overhead technique at the cost of losing precision -
// completely acceptable for types like characters and booleans but having to
// live with 61bit numerics instead of native 64 and some overhead for
// operations like multiplication & division.

mod immediate {
    use super::*;

    const NUM: i64 = 0;
    const BOOL: i64 = 1;
    const CHAR: i64 = 2;
    const NIL: i64 = 4;

    const SHIFT: i64 = 3;

    const FALSE: i64 = (0 << SHIFT) | BOOL;
    const TRUE: i64 = (1 << SHIFT) | BOOL;

    pub fn to(prog: &AST) -> i64 {
        match prog {
            AST::Number { i } => (i << SHIFT) | NUM,
            AST::Boolean { b: true } => TRUE,
            AST::Boolean { b: false } => FALSE,
            // An ASCII char is a single byte, so most of these shifts should be
            // OK. This is going to go wrong pretty badly with Unicode.
            AST::Char { c } => {
                // Expand u8 to i64 before shifting right, this will easily
                // overflow and give bogus results otherwise. Unit testing FTW!
                (i64::from(*c) << SHIFT) | CHAR
            }
            AST::Nil => NIL,
            AST::Identifier { .. } => unimplemented!(),
            AST::List { .. } => unimplemented!(),
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        const MASK: i64 = 0b00000111;

        // As of now, there is no need for this function in Rust other than
        // testing, but good to have :) There is an equivalent C implementation
        // to pretty print the values. Leaving this along with `to` leads to
        // dead code warnings.
        //
        // TODO: Switch to match, rely on exhaustive pattern matching rather
        // than the panic in the end.
        pub fn from(val: i64) -> AST {
            if (val & MASK) == NUM {
                return AST::Number { i: (val >> SHIFT) };
            } else if (val & MASK) == CHAR {
                return AST::Char {
                    c: (val >> SHIFT) as u8,
                };
            } else if val == TRUE {
                return AST::Boolean { b: true };
            } else if val == FALSE {
                return AST::Boolean { b: false };
            } else if val == NIL {
                return AST::Nil;
            } else {
                panic!("Oops");
            }
        }

        #[test]
        fn numbers() {
            assert_eq!(to(&AST::Number { i: 0 }), 0);
            assert_eq!(to(&AST::Number { i: 1 }), 8);

            assert_eq!(from(0), (AST::Number { i: 0 }));
            assert_eq!(from(8), (AST::Number { i: 1 }));
        }

        #[test]
        fn chars() {
            let expect = (65 << SHIFT) + CHAR;

            assert_eq!(to(&AST::Char { c: 'A' as u8 }), expect);
            assert_eq!(from(expect), AST::Char { c: 'A' as u8 });
        }
    }
}
