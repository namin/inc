//! # Inc
//!
//! Incremental approach to compiler construction.
//!
//!
use nom::types::CompleteByteSlice as S;
use std::fs::File;
use std::io::Write;
use std::str::FromStr;

/// Control behavior and external interaction of the program.
pub struct Config {
    /// Program is the input source
    pub program: String,
    /// Name of the generated asm and executable, stdout otherwise
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

/// Custom error type for all of inc.
// This might not be idiomatic Rust, revisit later.
#[derive(Debug)]
pub struct Error {
    message: String,
}

/// The LISP AST
///
/// The canonical type to represent lisp programs. The parser parses the input
/// program to generate an AST. See tests for several examples.
//
// TODO: Implement `Display` trait to pretty print the AST
#[derive(Debug, PartialEq)]
pub enum AST {
    Nil,
    Number(i64),
    Boolean(bool),
    /// A unicode char encoded in UTF-8 can take upto 4 bytes and won't fit in a
    /// word; so this implementation makes sense only for ASCII.
    Char(u8),
    Identifier(String),
    /// Since Rust needs to know the size of the AST type upfront, we need an
    /// indirection here with `Vec<>` for recursive types. In this context, Vec
    /// is just a convenient way to have a `Box<[AST]>`
    List(Vec<AST>),

    Let {
        bindings: Vec<(String, AST)>,
        body: Vec<AST>,
    },
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

/// A scheme parser in nom.
///
/// See http://www.scheme.com/tspl2d/grammar.html for formal grammar
/// specification. This module tries to describe this BNF grammar in Rust as
/// closely as posible using the nom parser combinator library.
///
/// Ported from https://github.com/jaseemabid/lisper/blob/master/src/Lisper/Parser.hs
///
pub mod parser {

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
          s: many0!(subsequent) >>
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
        boolean    => { |b| AST::Boolean(b) }   |
        ascii      => { |c| AST::Char(c) }      |
        number     => { |i| AST::Number(i) }    |
        identifier => { |i| AST::Identifier(i) }|
        list
    ));

    // <list> → (<datum>*) | (<datum>+ . <datum>) | <abbreviation>
    named!(list <S, AST>, do_parse!(
        char!('(') >>
        opt!(many0!(space)) >>
        d: map!(separated_list!(space, datum),
                {|ls: Vec<AST> |
                 if ls.is_empty() {
                     AST::Nil
                 } else {
                     AST::List(ls)
                 }}) >>
        opt!(many0!(space)) >>
        char!(')') >>
        (d)));

    named!(pub program <S, AST>, alt!(let_syntax | datum));

    // named → (name value)
    named!(binding <S, (String, AST)>, do_parse!(
        opt!(many0!(space)) >>
        char!('(') >>
        opt!(many0!(space)) >>
        name: identifier >>
        opt!(many0!(space)) >>
        value: datum >>
        opt!(many0!(space)) >>
        char!(')') >>
        opt!(many0!(space)) >>
        ((name, value))));

    // (let-syntax (<syntax binding>*) <expression>+)
    named!(let_syntax <S, AST>, do_parse!(
        char!('(') >>
        opt!(many0!(space)) >>
        tag!("let") >>
        opt!(many0!(space)) >>
        char!('(') >>
        b: many0!(binding) >>
        char!(')') >>
        opt!(many0!(space)) >>
        e: many1!(program) >>
        opt!(many0!(space)) >>
        char!(')') >>
      (AST::Let{bindings: b, body: e})));

    #[cfg(test)]
    mod tests {
        use super::*;

        // The complete input is parsed and there is nothing left.
        const EMPTY: S<'static> = S(b"");

        // OK consumes all of the input and succeeds
        fn ok<T>(t: T) -> Result<(S<'static>, T), nom::Err<S<'static>, u32>> {
            partial(EMPTY, t)
        }

        // Partial consumes some of the input and succeeds
        fn partial<T>(
            unconsumed: S<'static>,
            t: T,
        ) -> Result<(S<'_>, T), nom::Err<S<'_>, u32>> {
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

            // Character parser must not consume anything unless it starts with
            // an explicit tag.
            assert_eq!(fail(S(b"test")), ascii(S(b"test")));
        }

        #[test]
        fn identifiers() {
            assert_eq!(ok(String::from("x")), identifier(S(b"x")));
            assert_eq!(ok(String::from("one")), identifier(S(b"one")));
            assert_eq!(ok(String::from("!bang")), identifier(S(b"!bang")));
            assert_eq!(ok(String::from("a->b")), identifier(S(b"a->b")));
            assert_eq!(ok(String::from("+")), identifier(S(b"+")));
            assert_eq!(ok(String::from("-")), identifier(S(b"-")));
            assert_eq!(ok(String::from("i64")), identifier(S(b"i64")));

            // -> is not an identifier, consume the - as an id and return the >
            assert_eq!(
                partial(S(b">"), String::from("-")),
                identifier(S(b"->"))
            );

            // Identifiers must split at space and not consume anything
            // afterwards
            assert_eq!(
                partial(S(b" b"), String::from("a")),
                identifier(S(b"a b"))
            );
        }

        // #[test]
        // fn unicode() {
        //     assert_eq!(fail(S(b"അ")), identifier(S(b"അ")))
        // }

        #[test]
        fn data() {
            assert_eq!(ok(AST::Nil), datum(S(b"()")));
            assert_eq!(ok("one".into()), datum(S(b"one")));
            assert_eq!(ok(42.into()), datum(S(b"42")))
        }

        #[test]
        fn lists() {
            assert_eq!(
                ok(AST::List(vec!["+".into(), 1.into()])),
                list(S(b"(+ 1)"))
            );

            assert_eq!(
                ok(AST::List(vec![
                    1.into(),
                    2.into(),
                    3.into(),
                    "a".into(),
                    "b".into(),
                    "c".into()
                ])),
                list(S(b"(1 2 3 a b c)"))
            );

            assert_eq!(
                ok(AST::List(vec![
                    "inc".into(),
                    AST::List(vec!["inc".into(), 42.into()]),
                ],)),
                list(S(b"(inc (inc 42))"))
            );

            // Lists should throw away all spaces in between
            assert_eq!(program(S(b"(   +   1 )")), program(S(b"(+ 1)")));
        }

        #[test]
        fn binary() {
            assert_eq!(
                ok(AST::List(vec!["+".into(), "x".into(), 1776.into()])),
                list(S(b"(+ x 1776)"))
            );

            assert_eq!(
                ok(AST::List(vec![
                    "+".into(),
                    "x".into(),
                    AST::List(vec!["*".into(), "a".into(), "b".into()],),
                ],)),
                list(S(b"(+ x (* a b))"))
            );
        }

        #[test]
        fn top() {
            assert_eq!(ok(true.into()), program(S(b"#t")));
            assert_eq!(ok(false.into()), program(S(b"#f")));

            assert_eq!(ok('?'.into()), program(S(b"#\\?")));

            assert_eq!(ok(42.into()), program(S(b"42")));
            assert_eq!(ok((-42).into()), program(S(b"-42")));

            assert_eq!(ok('j'.into()), program(S(b"#\\j")));
            assert_eq!(ok('^'.into()), program(S(b"#\\^")));
        }

        #[test]
        fn let_binding() {
            let prog = S(b"(let ((x 1) (y 2)) (+ x y))");

            let exp = AST::Let {
                bindings: vec![
                    ("x".to_string(), AST::Number(1)),
                    ("y".to_string(), AST::Number(2)),
                ],
                body: vec![AST::List(vec![
                    AST::Identifier("+".to_string()),
                    AST::Identifier("x".to_string()),
                    AST::Identifier("y".to_string()),
                ])],
            };

            assert_eq!(ok(exp), program(prog));
        }
    }
}

/// A thin wrapper around x86 assembly.
///
/// This module should be a general purpose x86 library without importing
/// anything else from the rest of the compiler.
pub mod x86 {
    use std::fmt;
    use std::ops::{Add, AddAssign};

    /// ASM is a list of instructions.
    ///
    /// `Display` trait converts this type to valid code that can be compiled
    /// and executed. For now this is pretty dumb, but over time this could be
    /// made into something a lot smarter and safe rather than concatenating so
    /// many tiny strings together.
    #[derive(Clone)]
    pub struct ASM(pub Vec<Ins>);

    pub const WORDSIZE: i64 = 8;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Register {
        RAX,
        RBX,
        RCX,
        RDX,
        RSP,
        RBP,
    }

    /// Operand is a register, address or a constant; the argument to several
    /// instructions.
    ///
    /// This is an extremely simplified view of the reality; `mov` alone with
    /// the address access semantics x86 supports is Turning Complete.
    /// https://esolangs.org/wiki/Mov
    #[derive(Debug, Clone)]
    pub enum Operand {
        Const(i64),
        Reg(Register),
        Stack(i64),
    }

    /// Each x86 instruction this compiler understands.
    ///
    /// This type fundamentally limits what code can be generated and ideally no
    /// other part of the compiler should generate ASM with strings.
    #[derive(Debug, Clone)]
    pub enum Ins {
        /// Add `v` to register `r`
        Add { r: Register, v: Operand },

        /// Logical and of `v` to register `r`
        And { r: Register, v: Operand },

        /// Unconditional function call
        Call(String),

        /// Compare the value to register RAX
        Cmp { r: Register, with: i64 },

        /// x86 function preamble
        ///
        /// Not really a single instruction but having this as a single
        /// operation makes it easier for callers of this module.
        Enter,

        /// Jump to the specified label if last comparison resulted in equality
        Je(String),

        /// Unconditionally jump to the specified label
        Jmp(String),

        /// A label is a target to jump to
        Label(String),

        /// Exit a function and clean up. See `Enter`
        Leave,

        /// Save a register `r` to stack at index `si`
        Save { r: Register, si: i64 },

        /// Load a value at stack index `si` to register `r`
        Load { r: Register, si: i64 },

        /// Mov! At least one of the operands must be a register, moving from
        /// RAM to RAM isn't a valid op.
        Mov { from: Operand, to: Operand },

        /// Multiply register AX with value `v` and move result to register RAX
        // The destination operand is of `mul` is an implied operand located in
        // register AX. GCC throws `Error: ambiguous operand size for `mul'`
        // without size quantifier
        Mul { v: Operand },

        /// Pop a register `r` from stack
        Pop(Register),

        /// Push a register `r` to stack
        Push(Register),

        /// Return from the calling function
        Ret,

        // Shift Operations fall into `arithmetic` (`SAR` & `SAL`) and `logical`
        // (`SHR` & `SHL`) types and they differ in the way signs are preserved.
        //
        // Shifting left works the same for both because multiplying by 2^n wont
        // change the sign, but logical right shifting a negative number with
        // `SHR` will throw away the sign while `SAR` will preserve it. Prior
        // versions of this compiler and paper used both, but unless there is a
        // very good reason use shift arithmetic right (`SAR`) instead of shift
        // logical right (`SHR`) everywhere.
        /// Shift register `r` right by `v` bits; `r = r / 2^v`
        Sar { r: Register, v: i64 },

        /// Shift register `r` left by `v` bits; `r = r * 2^v`
        Sal { r: Register, v: i64 },

        /// Sub `k` from register `r`
        Sub { r: Register, v: Operand },

        /// Raw slices for compatibility
        ///
        /// Often it can be just convenient to hand write some assembly and
        /// eventually port it to a sensible type here. Till then this Variant
        /// is a goods stop gap.
        Slice(String),
    }

    /// Convert a single operation to ASM
    impl From<Ins> for ASM {
        fn from(op: Ins) -> Self {
            ASM { 0: vec![op] }
        }
    }

    /// Convert a String to ASM
    impl From<String> for ASM {
        fn from(s: String) -> Self {
            ASM { 0: vec![Ins::Slice(s)] }
        }
    }

    /// Convert a string literal to ASM
    impl From<&str> for ASM {
        fn from(s: &str) -> Self {
            ASM { 0: vec![Ins::Slice(s.to_string())] }
        }
    }

    /// Display for a register is the same as Debug
    impl fmt::Display for Register {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", format!("{:?}", self).to_lowercase())
        }
    }

    /// Display an Operand
    impl fmt::Display for Operand {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match &self {
                Operand::Const(i) => write!(f, "{}", i),
                Operand::Reg(r) => write!(f, "{}", r),
                Operand::Stack(si) => writeln!(f, "{}", &stack(*si)),
            }
        }
    }

    /// Pretty print a single ASM instruction.
    impl fmt::Display for Ins {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                Ins::Add { r, v } => writeln!(f, "    add {}, {}", r, v),
                Ins::And { r, v } => writeln!(f, "    and {}, {}", r, v),
                Ins::Call(l) => writeln!(f, "    call {}", l),
                Ins::Cmp { r, with } => writeln!(f, "    cmp {}, {}", r, with),
                Ins::Enter => {
                    let op = Ins::Push(Register::RBP)
                        + Ins::Mov {
                            from: Operand::Reg(Register::RSP),
                            to: Operand::Reg(Register::RBP),
                        };
                    write!(f, "{}", op)
                }
                Ins::Je(l) => writeln!(f, "    je {}", l),
                Ins::Jmp(l) => writeln!(f, "    jmp {}", l),
                Ins::Label(l) => writeln!(f, "{}", label(l)),
                Ins::Leave => {
                    let op = Ins::Pop(Register::RBP) + Ins::Ret;
                    writeln!(f, "{}", op)
                }
                Ins::Load { r, si } => {
                    writeln!(f, "    mov {}, {}", r, &stack(*si))
                }
                Ins::Mov { from, to } => {
                    writeln!(f, "    mov {}, {}", to, from)
                }
                Ins::Mul { v } => writeln!(f, "    mul qword ptr {}", v),
                Ins::Pop(r) => writeln!(f, "    pop {}", r),
                Ins::Push(r) => writeln!(f, "    push {}", r),
                Ins::Ret => writeln!(f, "    ret"),
                Ins::Save { r, si } => {
                    writeln!(f, "    mov {}, {}", &stack(*si), r)
                }
                Ins::Sal { r, v } => writeln!(f, "    sal {}, {}", r, v),
                Ins::Sar { r, v } => writeln!(f, "    sar {}, {}", r, v),
                Ins::Sub { r, v } => writeln!(f, "    sub {}, {}", r, v),
                Ins::Slice(s) => write!(f, "{}", s),
            }
        }
    }

    /// Collect all the bits and pieces together into valid assembly
    impl fmt::Display for ASM {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let mut ctx = String::new();
            for op in self.0.iter() {
                ctx.push_str(&op.to_string());
            }
            writeln!(f, "{}", ctx)
        }
    }

    /// Add operations with a easy to read `asm += op` short hand.
    ///
    /// This is pretty efficient at the cost of owning the value.
    impl AddAssign<Ins> for ASM {
        fn add_assign(&mut self, op: Ins) {
            self.0.push(op)
        }
    }

    /// Add operations to ASM with overloaded `asm' = asm + op`.
    ///
    /// NOTE: This is pretty inefficient due to copying of self.
    impl Add<Ins> for ASM {
        type Output = Self;

        fn add(self, op: Ins) -> Self {
            let mut t = self.clone();
            t.0.push(op);
            t
        }
    }

    /// Concat ASM; `asm + asm`
    ///
    /// NOTE: This is pretty inefficient due to copying both arguments.
    impl Add<ASM> for ASM {
        type Output = Self;

        fn add(self, asm: ASM) -> Self {
            let mut rhs = self.clone();
            let mut lhs = asm.0.clone();
            rhs.0.append(&mut lhs);
            rhs
        }
    }

    /// Concat Ins to get ASM; `asm = op + op`
    ///
    /// NOTE: This is pretty inefficient due to copying both arguments.
    impl Add<Ins> for Ins {
        type Output = ASM;

        fn add(self, op: Ins) -> ASM {
            ASM { 0: vec![self, op] }
        }
    }

    // ¶ Module helpers

    #[cfg(target_os = "macos")]
    fn label(label: &str) -> String {
        format!("_{}:", label)
    }

    #[cfg(target_os = "linux")]
    fn label(label: &str) -> String {
        format!("{}:", label)
    }

    /// Stack gives stack address relative to base pointer
    pub fn stack(si: i64) -> String {
        match si {
            index if index > 0 => format!("[rbp + {}]", index),
            index if index < 0 => format!("[rbp - {}]", (-index)),
            _ => panic!("Effective stack index cannot be 0"),
        }
    }

    #[cfg(target_os = "macos")]
    pub fn function_header(name: &str) -> ASM {
        let mut ctx = String::new();

        ctx.push_str("    .section __TEXT,__text\n");
        ctx.push_str("    .intel_syntax noprefix\n");
        ctx.push_str(&format!("    .globl _{}\n", &name));
        ctx.push_str(&Ins::Label(String::from(name)).to_string());
        ctx.into()
    }

    #[cfg(target_os = "linux")]
    pub fn function_header(name: &str) -> ASM {
        let mut ctx = String::new();

        ctx.push_str("    .text\n");
        ctx.push_str("    .intel_syntax noprefix\n");
        ctx.push_str(&format!("    .globl {}\n", &name));
        ctx.push_str(&format!("    .type {}, @function\n", &name));
        ctx.push_str(&Ins::Label(String::from(name)).to_string());
        ctx.into()
    }

    #[cfg(test)]
    mod tests {
        use crate::x86::{Ins::*, Operand::*, Register::*};

        #[test]
        fn mov() {
            assert_eq!(
                String::from("    mov rax, 16\n"),
                Mov { from: Const(16), to: Reg(RAX) }.to_string()
            );
        }
    }
}

/// Emit machine code for inc AST.
///
/// This module implements bulk of the compiler and is a good place to start
/// reading code. Platform specific code is annotated with `cfg(target_os)` for
/// both linux and mac. This module implements code gen specific to inc and
/// anything generic goes into `x86` module.
pub mod emit {
    use super::{
        immediate,
        x86::{Ins::*, Operand::*, Register::*, *},
        *,
    };

    /// State for the code generator; easier to bundle it all into a struct than
    /// pass several arguments in.
    ///
    /// Stack index points to the current available empty slot. Use and then
    /// decrement the index to add a new variable. Default to `-word size`
    ///
    /// State should also implement some form of register allocation.
    pub struct State {
        pub si: i64,
        pub asm: ASM,
        pub env: Env,
    }

    impl Default for State {
        fn default() -> Self {
            State { si: -WORDSIZE, asm: ASM(vec![]), env: Env(vec![]) }
        }
    }

    impl State {
        /// Get the next stack index for allocation
        //
        //TODO: This function is pretty inefficient
        pub fn next(&self) -> State {
            State {
                asm: self.asm.clone(),
                si: self.si - WORDSIZE,
                env: self.env.clone(),
            }
        }
    }

    // NOTE: Bindings and env would be a really good candidate for a new module
    // which hides implementation details.

    /// A binding maps an Identifier to a stack index.
    // TODO: Binding should *NOT* have an owned identifier, use a borrowed
    // reference here that doesn't outlive the real variable.
    #[derive(Debug, Clone)]
    pub struct Binding(String, i64);

    /// Environment is an *ordered* list of bindings.
    // A pair of (name, value) is a crappy data structure for an env, a list of
    // maps would be safer and idiomatic.
    #[derive(Debug, Clone)]
    pub struct Env(pub Vec<Binding>);

    impl Env {
        /// Grow an environment by pushing a new binding.
        fn grow(&mut self, i: String, index: i64) {
            self.0.push(Binding(i.to_string(), index));
        }

        /// Check if a variable exists in the env
        fn find(&self, i: &str) -> Option<i64> {
            for Binding(name, index) in self.0.iter() {
                if name == i {
                    return Some(*index);
                }
            }
            None
        }
    }

    /// Clear (mask) all except the least significant 3 tag bits
    pub fn mask() -> Ins {
        And { r: RAX, v: Const(immediate::MASK) }
    }

    /// Convert the result in RAX into a boolean
    pub fn cmp_bool() -> ASM {
        // SETE sets the destination operand to 0 or 1 depending on the settings
        // of the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
        (String::from("    sete al \n") +

         // MOVZX copies the contents of the source operand (register or
         // memory location) to the destination operand (register) and zero
         // extends the value.
         "    movzx rax, al \n" +
         &format!("    sal al, {} \n", immediate::SHIFT) +
         &format!("    or al, {} \n", immediate::BOOL))
            .into()
    }

    /// Emit code for a let expression
    ///
    /// A new environment is created to hold the bindings, which map the name to
    /// a stack index. All the space allocated by the let expression for local
    /// variables can be freed at the end of the body. This implies the `si`
    /// stays the same before and after a let expression. There is no need to
    /// keep track of the amount of space allocated inside the let expression
    /// and free it afterwards.

    pub fn binding(s: &State, bindings: &[(String, AST)], body: &[AST]) -> ASM {
        let mut ctx = String::new();
        let mut env = s.env.clone();
        let mut index = s.si;

        for (name, expr) in bindings.iter() {
            let x = eval(&s, expr) + Save { r: RAX, si: index };

            // TODO: Use something like index * WORDSIZE
            // here instead of mutating index.
            env.grow(name.to_string(), index);
            index -= WORDSIZE;

            ctx.push_str(&x.to_string());
        }

        let s2 = State { si: index, asm: s.asm.clone(), env };
        let x = eval(&s2, &body[0]);

        ctx.push_str(&x.to_string());
        ctx.into()
    }

    /// Evaluate an expression into RAX
    ///
    /// If the expression fits in a machine word, immediately return with the
    /// immediate repr, recurse for anything else till the base case.
    ///
    // TODO: eval should dispatch based on first atom alone, not necessarily
    // care about arity here. `let` and other variadic syntax forms won't fit
    // into any specific branch here.
    pub fn eval(s: &State, prog: &AST) -> ASM {
        match prog {
            AST::Identifier(i) => match s.env.find(i) {
                Some(i) => Ins::Load { r: Register::RAX, si: i }.into(),
                None => panic!("Undefined variable {}", i),
            },

            AST::Let { bindings, body } => binding(&s, bindings, body),

            AST::List(list) => match list.as_slice() {
                [AST::Identifier(i), arg] => match &i[..] {
                    "inc" => primitives::inc(&s, arg),
                    "dec" => primitives::dec(&s, arg),
                    "null?" => primitives::nullp(&s, arg),
                    "zero?" => primitives::zerop(&s, arg),
                    "not" => primitives::not(&s, arg),
                    "fixnum?" => primitives::fixnump(&s, arg),
                    "boolean?" => primitives::booleanp(&s, arg),
                    "char?" => primitives::charp(&s, arg),
                    n => panic!("Unknown unary primitive: {}", n),
                },

                [AST::Identifier(name), x, y] => match &name[..] {
                    "+" => primitives::plus(&s, x, y),
                    "-" => primitives::minus(&s, x, y),
                    "*" => primitives::mul(&s, x, y),
                    "/" => primitives::quotient(&s, x, y),
                    "%" => primitives::remainder(&s, x, y),

                    n => panic!("Unknown binary primitive: {}", n),
                },

                l => panic!("Unknown expression: {:?}", l),
            },

            _ => Mov {
                to: Operand::Reg(RAX),
                from: Operand::Const(immediate::to(&prog)),
            }
            .into(),
        }
    }

    /// Top level interface to the emit module
    pub fn program(prog: &AST) -> String {
        let s: State = Default::default();
        let gen = x86::function_header("init")
            + Ins::Enter
            + eval(&s, prog)
            + Ins::Leave;

        gen.to_string()
    }
}

/// Scheme primitives implemented directly in the compiler
///
/// Several scheme functions like `(add ...` are implemented by the compiler in
/// assembly rather than in scheme. All of them live in this module.
pub mod primitives {

    use super::emit::State;
    use super::x86::{Ins::*, Register::*, *};
    use super::*;

    // Unary Primitives

    /// Increment number by 1
    pub fn inc(s: &State, x: &AST) -> ASM {
        emit::eval(&s, x) + Add { r: RAX, v: Operand::Const(immediate::n(1)) }
    }

    /// Decrement by 1
    pub fn dec(s: &State, x: &AST) -> ASM {
        emit::eval(&s, x) + Sub { r: RAX, v: Operand::Const(immediate::n(1)) }
    }

    /// Is the expression a fixnum?
    ///
    /// # Examples
    ///
    /// ```scheme
    /// (fixnum? 42) => #t
    /// (fixnum? "hello") => #f
    /// ```
    pub fn fixnump(s: &State, expr: &AST) -> ASM {
        emit::eval(&s, expr)
            + emit::mask()
            + Cmp { r: RAX, with: immediate::NUM }
            + emit::cmp_bool()
    }

    /// Is the expression a boolean?
    pub fn booleanp(s: &State, expr: &AST) -> ASM {
        emit::eval(&s, expr)
            + emit::mask()
            + Cmp { r: RAX, with: immediate::BOOL }
            + emit::cmp_bool()
    }

    /// Is the expression a char?
    pub fn charp(s: &State, expr: &AST) -> ASM {
        emit::eval(&s, expr)
            + emit::mask()
            + Cmp { r: RAX, with: immediate::CHAR }
            + emit::cmp_bool()
    }

    /// Is the expression null?
    pub fn nullp(s: &State, expr: &AST) -> ASM {
        emit::eval(&s, expr)
            + Cmp { r: RAX, with: immediate::NIL }
            + emit::cmp_bool()
    }

    /// Is the expression zero?
    pub fn zerop(s: &State, expr: &AST) -> ASM {
        emit::eval(&s, expr)
            + Cmp { r: RAX, with: immediate::NUM }
            + emit::cmp_bool()
    }

    /// Logical not
    pub fn not(s: &State, expr: &AST) -> ASM {
        emit::eval(&s, expr)
            + Cmp { r: RAX, with: immediate::FALSE }
            + emit::cmp_bool()
    }

    // Binary Primitives

    /// Evaluate arguments for a binary primitive and store them in stack
    fn binop(s: &emit::State, x: &AST, y: &AST) -> ASM {
        emit::eval(&s, x) + Save { r: RAX, si: s.si } + emit::eval(&s.next(), y)
    }

    /// Add `x` and `y` and move result to register RAX
    pub fn plus(s: &State, x: &AST, y: &AST) -> ASM {
        binop(&s, &x, &y) + Add { r: RAX, v: Operand::Stack(s.si) }
    }

    /// Subtract `x` from `y` and move result to register RAX
    //
    // `sub` Subtracts the 2nd op from the first and stores the result in the
    // 1st. This is pretty inefficient to update result in stack and load it
    // back. Reverse the order and fix it up.
    pub fn minus(s: &State, x: &AST, y: &AST) -> ASM {
        binop(&s, &x, &y)
            + Sub { r: RAX, v: Operand::Stack(s.si) }
            + Load { r: RAX, si: s.si }
    }

    /// Multiply `x` and `y` and move result to register RAX
    // The destination operand is of `mul` is an implied operand located in
    // register AX. GCC throws `Error: ambiguous operand size for `mul'` without
    // size quantifier
    pub fn mul(s: &State, x: &AST, y: &AST) -> ASM {
        binop(&s, &x, &y)
            + Sar { r: RAX, v: immediate::SHIFT }
            + Mul { v: Operand::Stack(s.si) }
    }

    /// Divide `x` by `y` and move result to register RAX
    // Division turned out to be much more trickier than I expected it to be.
    // Unlike @namin's code, I'm using a shift arithmetic right (SAR) instead of
    // shift logical right (SHR) and I don't know how the original examples
    // worked at all for negative numbers. I also had to use the CQO instruction
    // to Sign-Extend RAX which the 32 bit version is obviously not concerned
    // with. I got the idea from GCC disassembly.
    //
    // Dividend is passed in RDX:RAX and IDIV instruction takes the divisor as the
    // argument. the quotient is stored in RAX and the remainder in RDX.
    fn div(s: &State, x: &AST, y: &AST) -> ASM {
        let mut ctx = String::new();

        ctx.push_str(&(emit::eval(&s, y).to_string()));
        ctx.push_str(
            &Ins::Sar { r: Register::RAX, v: immediate::SHIFT }.to_string(),
        );
        ctx.push_str("    mov rcx, rax \n");
        ctx.push_str(&emit::eval(&s, x).to_string());
        ctx.push_str(
            &Ins::Sar { r: Register::RAX, v: immediate::SHIFT }.to_string(),
        );
        ctx.push_str("    mov rdx, 0 \n");
        ctx.push_str("    cqo \n");
        ctx.push_str("    idiv rcx \n");
        ctx.into()
    }

    pub fn quotient(s: &State, x: &AST, y: &AST) -> ASM {
        div(&s, x, y) + Sal { r: Register::RAX, v: immediate::SHIFT }
    }

    pub fn remainder(s: &State, x: &AST, y: &AST) -> ASM {
        div(&s, x, y)
            + Mov {
                to: Operand::Reg(Register::RAX),
                from: Operand::Reg(Register::RDX),
            }
            + Sal { r: Register::RAX, v: immediate::SHIFT }
    }
}

/// Runtime representation of typed scheme values
///
/// Immediate values (values that can be fit in one machine word) are tagged for
/// distinguising them from heap allocated pointers. The last 3 bits effectively
/// serve as the runtime type of the value. Always using 3 bits is a simpler
/// approach than the multi bit technique the paper uses. This is a very
/// efficient and low overhead technique at the cost of losing precision -
/// completely acceptable for types like characters and booleans but having to
/// live with 61bit numerics instead of native 64 and some overhead for
/// operations like multiplication & division.
///
/// See the paper for details. See tests for examples.
pub mod immediate {
    use super::*;

    pub const NUM: i64 = 0;
    pub const BOOL: i64 = 1;
    pub const CHAR: i64 = 2;
    pub const NIL: i64 = 4;

    pub const SHIFT: i64 = 3;
    pub const MASK: i64 = 0b0000_0111;

    pub const FALSE: i64 = (0 << SHIFT) | BOOL;
    pub const TRUE: i64 = (1 << SHIFT) | BOOL;

    /// Immediate representation of an expression.
    ///
    /// Immediate representation is only defined for some types and this
    /// function is partial. The caller for this function must make sure of it,
    /// rather than make this module complicated. It would be great if the type
    /// system could ensure that, but till then fail with a panic.
    pub fn to(prog: &AST) -> i64 {
        match prog {
            AST::Number(i) => (i << SHIFT) | NUM,
            AST::Boolean(true) => TRUE,
            AST::Boolean(false) => FALSE,
            // An ASCII char is a single byte, so most of these shifts should be
            // OK. This is going to go wrong pretty badly with Unicode.
            AST::Char(c) => {
                // Expand u8 to i64 before shifting right, this will easily
                // overflow and give bogus results otherwise. Unit testing FTW!
                (i64::from(*c) << SHIFT) | CHAR
            }
            AST::Nil => NIL,
            AST::Identifier(i) => unimplemented!(
                "immediate repr is undefined for identifier {}",
                i
            ),
            AST::List(..) => {
                unimplemented!("immediate repr is undefined for lists")
            }
            AST::Let { .. } => {
                unimplemented!("immediate repr is undefined for let binding")
            }
        }
    }

    // Immediate representation of numbers is required so often a helper is
    // useful.
    pub fn n(i: i64) -> i64 {
        (i << SHIFT) | NUM
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        // As of now, there is no need for this function in Rust other than
        // testing, but good to have :) There is an equivalent C implementation
        // to pretty print the values. Leaving this along with `to` leads to
        // dead code warnings.
        //
        // TODO: Switch to match, rely on exhaustive pattern matching rather
        // than the panic in the end.
        pub fn from(val: i64) -> AST {
            if (val & MASK) == NUM {
                return AST::Number(val >> SHIFT);
            } else if (val & MASK) == CHAR {
                return AST::Char((val >> SHIFT) as u8);
            } else if val == TRUE {
                return true.into();
            } else if val == FALSE {
                return false.into();
            } else if val == NIL {
                return AST::Nil;
            } else {
                panic!("Oops");
            }
        }

        #[test]
        fn numbers() {
            assert_eq!(to(&0.into()), 0);
            assert_eq!(to(&1.into()), 8);

            assert_eq!(from(0), 0.into());
            assert_eq!(from(8), 1.into());
        }

        #[test]
        fn chars() {
            let expect = (65 << SHIFT) + CHAR;

            assert_eq!(to(&('A').into()), expect);
            assert_eq!(from(expect), 'A'.into());
        }
    }
}

/// Parse the input from user into the form the top level of the compiler
/// understands.
impl FromStr for AST {
    type Err = Error;

    fn from_str(program: &str) -> Result<Self, Error> {
        match parser::program(S(program.as_bytes())) {
            Ok((_rest, ast)) => Ok(ast),
            // Ok((parser::EMPTY, ast)) => Ok(ast),
            // Ok((_rest, _ast)) => Err(Error {
            //     message: String::from("All of input not consumed"),
            // }),
            Err(e) => Err(Error { message: format!("{}", e) }),
        }
    }
}

/// Top level API for inc
///
/// Compile a scheme program into x86 asm; input program and output file is
/// passed with Config.
pub fn compile(config: &mut Config) -> Result<(), Error> {
    let prog: AST = config.program.parse::<AST>()?;

    let mut handler = File::create(&config.asm())
        .unwrap_or_else(|_| panic!("Failed to create {}", &config.asm()));

    match handler.write_all(emit::program(&prog).as_bytes()) {
        Ok(_) => Ok(()),
        Err(e) => Err(Error {
            message: format!("Failed to write generated code: {}", e),
        }),
    }
}
