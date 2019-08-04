//! Core types shared by most of the program

/// The canonical type to represent a lisp program.

/// The parser parses the input program to generate an AST. See tests for
/// several examples.
use std::fmt;

/// Abstract Syntax Tree for a single expression
#[derive(Debug, PartialEq)]
pub enum AST {
    /// An empty list `()`
    Nil,
    /// 61b number with a 3bit tag
    Number(i64),
    /// #t & #f
    Boolean(bool),
    /// A unicode char encoded in UTF-8 can take upto 4 bytes and won't fit in a
    /// word; so this implementation makes sense only for ASCII.
    Char(u8),
    /// UTF-8 Strings
    Str(String),
    /// Scheme Identifiers
    Identifier(String),
    /// Since Rust needs to know the size of the AST type upfront, we need an
    /// indirection here with `Vec<>` for recursive types. In this context, Vec
    /// is just a convenient way to have a `Box<[AST]>`
    List(Vec<AST>),
    /// Conditional
    Cond { pred: Box<AST>, then: Box<AST>, alt: Option<Box<AST>> },
    /// Variable bindings
    Let { bindings: Vec<(String, AST)>, body: Vec<AST> },
    /// Functions
    Lambda { args: Vec<String>, body: Vec<AST> },
}

/// Expressions wrap over `Vec<T>` so new traits can be defined on it
#[derive(Debug)]
pub struct Expressions(pub Vec<AST>);

/// Pretty print an AST
impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AST::Nil => write!(f, "()"),
            AST::Number(n) => write!(f, "{}", n),
            AST::Boolean(t) => write!(f, "{}", if *t { "#t" } else { "#f" }),
            AST::Char(c) => write!(f, "{}", c),
            AST::Str(s) => write!(f, "\"{}\"", s),
            AST::Identifier(i) => write!(f, "{}", i),
            AST::List(l) => {
                write!(f, "(")?;
                for i in l {
                    write!(f, "{} ", i)?;
                }
                write!(f, ")")
            }
            AST::Cond { pred, then, alt } => match alt {
                None => write!(f, "(if {} {})", pred, then),
                Some(t) => write!(f, "(if {} {} {})", pred, then, t),
            },
            AST::Let { bindings, body } => {
                write!(f, "(let (")?;
                for (a, b) in bindings {
                    write!(f, "({} {}) ", a, b)?;
                }
                write!(f, ") ")?;

                for b in body {
                    write!(f, "{}", b)?;
                }

                write!(f, ")")
            }
            AST::Lambda { args, body } => {
                write!(f, "(λ (")?;
                for arg in args {
                    write!(f, "{} ", arg)?;
                }
                write!(f, ") ")?;

                for b in body {
                    write!(f, "{}", b)?;
                }

                write!(f, ")")
            }
        }
    }
}

impl fmt::Display for Expressions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for line in &self.0 {
            write!(f, "{}", line).unwrap();
        }
        Ok(())
    }
}

/// Idiomatic type conversions from the primitive types to AST
///
/// https://doc.rust-lang.org/rust-by-example/conversion/from_into.html
/// https://ricardomartins.cc/2016/08/03/convenient_and_idiomatic_conversions_in_rust
impl From<i64> for AST {
    fn from(i: i64) -> Self {
        AST::Number(i)
    }
}

impl From<bool> for AST {
    fn from(b: bool) -> Self {
        AST::Boolean(b)
    }
}

impl From<char> for AST {
    fn from(c: char) -> Self {
        AST::Char(c as u8)
    }
}

impl From<&str> for AST {
    fn from(i: &str) -> Self {
        AST::Identifier(String::from(i))
    }
}

/// Control behavior and external interaction of the program.
pub struct Config {
    /// Program is the input source
    pub program: String,
    /// Name of the generated asm and executable, stdout otherwise
    pub output: String,
    /// Execute?
    pub exec: bool,
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

/// Custom error type for all of inc.
// This might not be idiomatic Rust, revisit later.
// https://doc.rust-lang.org/std/error/trait.Error.html
// https://learning-rust.github.io/docs/e7.custom_error_types.html
// https://doc.rust-lang.org/beta/rust-by-example/error/multiple_error_types/define_error_type.html
// https://medium.com/@fredrikanderzon/custom-error-types-in-rust-and-the-operator-b499d0fb2925
#[derive(Debug)]
pub struct Error {
    pub message: String,
}
