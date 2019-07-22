//! Inc Compiler

/// State for the code generator
pub mod state {
    use crate::x86::{ASM, WORDSIZE};
    use std::collections::HashMap;

    /// State for the code generator; easier to bundle it all into a struct than
    /// pass several arguments in.
    ///
    /// Stack index points to the current available empty slot. Use and then
    /// decrement the index to add a new variable. Default to `-word size`
    ///
    /// `li` is label index, a counter used to generate unique labels. See
    /// `gen_label`
    ///
    /// `symbols` is a list of all strings known at compile time, so that they
    /// can be allocated in the binary instead of heap.
    ///
    /// State should also implement some form of register allocation.
    pub struct State {
        pub si: i64,
        pub asm: ASM,
        li: u64,
        pub symbols: HashMap<String, usize>,
        env: Env,
    }

    impl Default for State {
        fn default() -> Self {
            State {
                si: -WORDSIZE,
                asm: ASM(vec![]),
                li: 0,
                symbols: HashMap::new(),
                env: new(),
            }
        }
    }

    impl State {
        pub fn enter(&mut self) {
            self.env.enter();
        }

        pub fn leave(&mut self) {
            let unwind = self.env.0.first().expect("unexpected empty env").len()
                as i64
                * WORDSIZE;
            self.si += unwind;
            self.env.leave()
        }

        pub fn get(&mut self, i: &str) -> Option<i64> {
            self.env.get(i)
        }

        // Set a new binding in the current local environment
        pub fn set(&mut self, i: &str, index: i64) {
            self.env.set(i, index);
            self.alloc();
        }

        /// Allocate a word on the stack & return reference to existing empty slot
        ///
        /// Since stack index points to existing free memory, it is useful to be
        /// able to use it and increment in one go.
        ///
        /// # Example:
        ///
        /// ```ignore
        /// Save { r: RAX, si: s.alloc() }
        /// ```
        pub fn alloc<'a>(&'a mut self) -> i64 {
            let current = self.si;
            self.si -= WORDSIZE;
            current
        }

        /// Explicitly free `n` words of memory from stack
        pub fn dealloc<'a>(&'a mut self, count: i64) {
            self.si += count * WORDSIZE;
        }

        /// Generate a unique label for jump targets.
        pub fn gen_label(&mut self) -> String {
            self.li += 1;
            format!("L_{}", self.li)
        }
    }
    // Environment is an *ordered* list of bindings.
    #[derive(Debug)]
    struct Env(Vec<HashMap<String, i64>>);

    fn new() -> Env {
        Env(vec![HashMap::new()])
    }

    impl Env {
        pub fn enter(&mut self) {
            self.0.insert(0, HashMap::new());
        }

        pub fn leave(&mut self) {
            self.0.remove(0);
        }

        pub fn set(&mut self, i: &str, index: i64) {
            self.0
                .first_mut()
                .map(|binding| binding.insert(i.to_string(), index));
        }

        pub fn get(&mut self, i: &str) -> Option<i64> {
            for bindings in self.0.iter() {
                if let Some(t) = bindings.get(i) {
                    return Some(*t);
                }
            }
            None
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn t() {
            let mut e = new();
            assert_eq!(e.0.len(), 1);

            // default global scope
            e.set("x", -8);
            assert_eq!(e.get("x"), Some(-8));

            // overwrite in current scope
            e.set("x", -16);
            assert_eq!(e.get("x"), Some(-16));

            e.enter();
            assert_eq!(e.0.len(), 2);
            // read variables from parent scope
            assert_eq!(e.get("x"), Some(-16));

            e.set("y", -24);
            // local variable shadows global
            e.set("x", -32);
            assert_eq!(e.get("x"), Some(-32));

            e.leave();

            assert_eq!(e.0.len(), 1);
            assert_eq!(e.get("y"), None);
            assert_eq!(e.get("x"), Some(-16));
        }
    }
}

/// A string is a blob of UTF-8 encoded bytes prefixed with the length if it.
///
/// Strings can be stack or heap allocated but static strings found in the
/// source code is retained as it is in the data section.
///
/// Example memory layout:
///
/// A literal "hello world" gets statically allocated at offset 4000 along with
/// the length. There is no extra allocation required for the string object
/// after immediate tagging the address as 4005 `(4000 | strtag)`.
///
/// ```txt
///  -------------------------
/// | Address | Value         |
///  -------------------------
/// | 4000    | 11            |
/// | 4000    | "hello world" |
/// |         |               |
/// | 8000    | 4005          |
///  -------------------------
/// ```
///
/// The C runtime would get the value 4005 and would identify it as a string
/// with the tag `(8005 & mask == strtag)`. The raw pointer is obtained by
/// removing the tag `(p = val - strtag)` and length is found at the base
/// addresses `(*p)` and the data at `(*p + 1)`. fwrite can safely print the
/// exact number of bytes using the length and pointer.
///
/// TODO: Consider switching to SDS. https://github.com/antirez/sds
pub mod string {

    use crate::{
        compiler::state::State,
        core::AST::{self, *},
        immediate,
        x86::{Ins::*, Register::*, ASM},
    };

    pub fn eval(s: &State, data: &str) -> ASM {
        let index = s.symbols.get(data);
        let index = index
            .expect(&format!("String `{}` not found in symbol table", data));

        Lea { r: RAX, of: format!("inc_str_{}", index), offset: immediate::STR }
            .into()
    }

    fn label(index: &usize) -> String {
        format!("inc_str_{}", index)
    }

    pub fn emit_symbols(s: &State) -> ASM {
        let mut asm = ASM(vec![]);

        for (symbol, index) in s.symbols.iter() {
            // `.p2align 3` aligns the address of the following target to 8
            // bytes by setting the 3 low order bits to 0. This is necessary for
            // the immediate tagging scheme to work correctly.
            //
            // https://sourceware.org/binutils/docs-2.32/as/P2align.html
            asm += Slice("    .p2align 3 \n".to_string());
            asm += Label(label(index));
            asm += Slice(format!("    .quad  {} \n", symbol.len()));
            asm += Slice(format!("    .ascii \"{}\" \n", symbol))
        }

        asm
    }

    pub fn lift(s: &mut State, prog: &AST) {
        match prog {
            Str(reference) => {
                if !s.symbols.contains_key(reference) {
                    s.symbols.insert(reference.clone(), s.symbols.len());
                }
            }

            Let { bindings, body } => {
                for (_name, expr) in bindings.iter() {
                    lift(s, expr);
                }

                for b in body.iter() {
                    lift(s, b)
                }
            }

            List(list) => {
                for l in list.iter() {
                    lift(s, l);
                }
            }
            _ => {}
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
    use crate::{
        compiler::state::State,
        compiler::string,
        core::AST::{self, *},
        immediate, primitives,
        x86::{
            self,
            Ins::{self, *},
            Operand::*,
            Register::*,
            ASM,
        },
    };

    /// Clear (mask) all except the least significant 3 tag bits
    pub fn mask() -> Ins {
        And { r: RAX, v: Const(immediate::MASK) }
    }

    /// Emit code for a let expression
    ///
    /// A new environment is created to hold the bindings, which map the name to
    /// a stack index. All the space allocated by the let expression for local
    /// variables can be freed at the end of the body. This implies the `si`
    /// stays the same before and after a let expression. There is no need to
    /// keep track of the amount of space allocated inside the let expression
    /// and free it afterwards.
    pub fn binding(s: &mut State, vars: &[(String, AST)], body: &[AST]) -> ASM {
        let mut asm = ASM(vec![]);

        s.enter();

        for (name, expr) in vars.iter() {
            asm += eval(s, expr) + Save { r: RAX, si: s.si };
            s.set(name, s.si);
        }

        for b in body.iter() {
            asm += eval(s, &b);
        }

        s.leave();
        asm
    }

    /// Emit code for a conditional expression
    pub fn cond(s: &mut State, predicate: &AST, then: &AST, alt: &AST) -> ASM {
        let alt_label = s.gen_label();
        let exit_label = s.gen_label();

        eval(s, predicate)
            + Ins::Cmp { a: Reg(RAX), b: Const(immediate::FALSE) }
            + Ins::Je(alt_label.clone())
            + eval(s, then)
            + Ins::Jmp(exit_label.clone())
            + Ins::Label(alt_label)
            + eval(s, alt)
            + Ins::Label(exit_label)
    }

    /// Evaluate an expression into RAX
    ///
    /// If the expression fits in a machine word, immediately return with the
    /// immediate repr, recurse for anything else till the base case.
    ///
    // TODO: eval should dispatch based on first atom alone, not necessarily
    // care about arity here. `let` and other variadic syntax forms won't fit
    // into any specific branch here.
    pub fn eval(s: &mut State, prog: &AST) -> ASM {
        match prog {
            Identifier(i) => match s.get(i) {
                Some(i) => Ins::Load { r: RAX, si: i }.into(),
                None => panic!("Undefined variable {}", i),
            },

            // Find the symbol index and return and reference in RAX
            Str(data) => string::eval(&s, &data),

            Let { bindings, body } => binding(s, bindings, body),

            List(list) => match list.as_slice() {
                [Identifier(i), arg] => match &i[..] {
                    "inc" => primitives::inc(s, arg),
                    "dec" => primitives::dec(s, arg),
                    "null?" => primitives::nullp(s, arg),
                    "zero?" => primitives::zerop(s, arg),
                    "not" => primitives::not(s, arg),
                    "fixnum?" => primitives::fixnump(s, arg),
                    "boolean?" => primitives::booleanp(s, arg),
                    "pair?" => primitives::pairp(s, arg),
                    "char?" => primitives::charp(s, arg),
                    "car" => primitives::car(s, arg),
                    "cdr" => primitives::cdr(s, arg),
                    n => panic!("Unknown unary primitive: {}", n),
                },

                [Identifier(name), x, y] => match &name[..] {
                    "cons" => primitives::cons(s, x, y),
                    "+" => primitives::plus(s, x, y),
                    "-" => primitives::minus(s, x, y),
                    "*" => primitives::mul(s, x, y),
                    "/" => primitives::quotient(s, x, y),
                    "%" => primitives::remainder(s, x, y),
                    "=" => primitives::eq(s, x, y),
                    ">" => primitives::gt(s, x, y),
                    "<" => primitives::lt(s, x, y),
                    ">=" => primitives::gte(s, x, y),
                    "<=" => primitives::lte(s, x, y),
                    n => panic!("Unknown binary primitive: {}", n),
                },

                [Identifier(name), x, y, z] => match &name[..] {
                    "if" => cond(s, x, y, z),
                    n => panic!("Unknown ternary primitive: {}", n),
                },

                l => panic!("Unknown expression: {:?}", l),
            },

            _ => Mov { to: Reg(RAX), from: Const(immediate::to(&prog)) }.into(),
        }
    }

    /// Top level interface to the emit module
    pub fn program(prog: &AST) -> String {
        let mut s: State = Default::default();
        string::lift(&mut s, prog);
        let gen = x86::prelude()
            + x86::func("init")
            + Enter
            + x86::init_heap()
            + eval(&mut s, prog)
            + Leave
            + string::emit_symbols(&s);

        gen.to_string()
    }
}
