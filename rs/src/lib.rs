// ---
// TODO:
//
// 1. Why is there no formatln! macro?
// 2. A nicer API to cat these tiny bits of strings would be great!
// ---

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
        format!("{}.s", self.output)
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
    Number { i: i64 },
    Boolean { b: bool },
    // A unicode char encoded in UTF-8 can take upto 4 bytes and won't fit in a
    // word; so this implementation makes sense only for ASCII.
    Char { c: u8 },
    Nil,
}

impl AST {
    pub fn t() -> AST {
        AST::Boolean { b: true }
    }

    pub fn f() -> AST {
        AST::Boolean { b: false }
    }
}

// Parse the input from user into the form the top level of the compiler
// understands.
//
// TODO: There must be much nice idiomatic ways to do this without unwrapping
// and wrapping the errors again.
//
// Migrate the simple hand written parser into something proper. `nom` maybe?

impl FromStr for AST {
    type Err = Error;

    fn from_str(program: &str) -> Result<Self, Error> {
        let marker = r"#\";

        match program.trim_end() {
            "#t" => Ok(AST::t()),
            "#f" => Ok(AST::f()),
            "()" => Ok(AST::Nil),

            // $ man ascii
            r"#\newline" => Ok(AST::Char { c: 10 }),
            r"#\return" => Ok(AST::Char { c: 13 }),
            r"#\space" => Ok(AST::Char { c: 32 }),
            r"#\tab" => Ok(AST::Char { c: 9 }),

            // Characters start with #\
            p if p.starts_with(marker) => match p.replace(marker, "").parse::<char>() {
                Ok(c) => {
                    // ASCII; NOTE: .is_ascii might be cleaner?
                    if c.len_utf8() == 1 {
                        Ok(AST::Char { c: c as u8 })
                    } else {
                        Err(Error {
                            message: String::from(format!("Failed to parse non ASCII char {}", c)),
                        })
                    }
                }
                Err(e) => Err(Error {
                    message: format!("Parse error for `{}`: {}", p, e),
                }),
            },

            n => match n.parse::<i64>() {
                Ok(i) => Ok(AST::Number { i }),
                Err(e) => Err(Error {
                    message: String::from(format!("Failed to parse program: {}", e)),
                }),
            },
        }
    }
}

#[cfg(test)]
mod parse {
    use super::*;

    fn void(_: AST) -> Result<(), Error> {
        Ok(())
    }

    #[test]
    fn number() -> Result<(), Error> {
        "42".parse().and_then(void)
    }

    #[test]
    fn bool() -> Result<(), Error> {
        "#t".parse().and_then(void)?;
        "#f".parse().and_then(void)
    }

    #[test]
    fn char() -> Result<(), Error> {
        r"#\a".parse().and_then(void)?;
        r"#\t".parse().and_then(void)?;
        r"#\^".parse().and_then(void)
    }

    #[test]
    fn uncode() {
        assert!("അ".parse::<AST>().is_err(), "Sadly no Unicode for now");
    }
}

pub fn compile(config: &mut Config) -> Result<(), Error> {
    let i: AST = config.program.parse::<AST>()?;

    let asm = format!("{}.s", &config.output);
    let mut handler = File::create(&asm).expect(&format!("Failed to create {}", &asm));

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

    pub fn program(prog: AST) -> String {
        let mut ctx = String::new();

        ctx.push_str(&function_header("_init")[..]);
        ctx.push_str(&format!("  movq ${}, %rax\n", immediate::to(prog)));
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

    pub fn to(prog: AST) -> i64 {
        match prog {
            AST::Number { i } => (i << SHIFT) | NUM,
            AST::Boolean { b: true } => TRUE,
            AST::Boolean { b: false } => FALSE,
            // An ASCII char is a single byte, so most of these shifts should be
            // OK. This is going to go wrong pretty badly with Unicode.
            AST::Char { c } => {
                // Expand u8 to i64 before shifting right, this will easily
                // overflow and give bogus results otherwise. Unit testing FTW!
                (i64::from(c) << SHIFT) | CHAR
            }
            AST::Nil => NIL,
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
            assert_eq!(to(AST::Number { i: 0 }), 0);
            assert_eq!(to(AST::Number { i: 1 }), 8);

            assert_eq!(from(0), (AST::Number { i: 0 }));
            assert_eq!(from(8), (AST::Number { i: 1 }));
        }

        #[test]
        fn chars() {
            let expect = (65 << SHIFT) + CHAR;

            assert_eq!(to(AST::Char { c: 'A' as u8 }), expect);
            assert_eq!(from(expect), AST::Char { c: 'A' as u8 });
        }
    }
}
