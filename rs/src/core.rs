//! Core types shared by most of the program

/// The canonical type to represent a lisp program.

/// The parser parses the input program to generate an AST. See tests for
/// several examples.
// TODO: Implement `Display` trait to pretty print the AST
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
    /// Variable bindings
    Let { bindings: Vec<(String, AST)>, body: Vec<AST> },
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
#[derive(Debug)]
pub struct Error {
    pub message: String,
}
