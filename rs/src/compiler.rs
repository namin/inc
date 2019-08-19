//! Inc Compiler

/// State for the code generator
pub mod state {
    use crate::x86::{ASM, WORDSIZE};
    use std::collections::{HashMap, HashSet};

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
    /// `functions` are all user defined functions
    ///
    /// State should also implement some form of register allocation.
    pub struct State {
        pub si: i64,
        pub asm: ASM,
        li: u64,
        pub symbols: HashMap<String, usize>,
        pub functions: HashSet<String>,
        env: Env,
    }

    impl Default for State {
        fn default() -> Self {
            State {
                si: -WORDSIZE,
                asm: Default::default(),
                li: 0,
                symbols: HashMap::new(),
                functions: HashSet::new(),
                env: Default::default(),
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

        pub fn get(&self, i: &str) -> Option<i64> {
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
        pub fn alloc(&mut self) -> i64 {
            let current = self.si;
            self.si -= WORDSIZE;
            current
        }

        /// Explicitly free `n` words of memory from stack
        pub fn dealloc(&mut self, count: i64) {
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

    impl Default for Env {
        fn default() -> Self {
            Env(vec![HashMap::new()])
        }
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

        pub fn get(&self, i: &str) -> Option<i64> {
            for bindings in &self.0 {
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
            let mut e: Env = Default::default();
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

/// Emit machine code for inc AST.
///
/// This module implements bulk of the compiler and is a good place to start
/// reading code. Platform specific code is annotated with `cfg(target_os)` for
/// both linux and mac. This module implements code gen specific to inc and
/// anything generic goes into `x86` module.
pub mod emit {
    use crate::{
        compiler::state::State,
        core::{
            Expr::{self, *},
            Expressions,
        },
        immediate, lambda, primitives, strings,
        x86::{self, Ins, Register::*, ASM},
    };

    /// Clear (mask) all except the least significant 3 tag bits
    pub fn mask() -> Ins {
        x86::and(RAX, immediate::MASK)
    }

    /// Emit code for a let expression
    ///
    /// A new environment is created to hold the bindings, which map the name to
    /// a stack index. All the space allocated by the let expression for local
    /// variables can be freed at the end of the body. This implies the `si`
    /// stays the same before and after a let expression. There is no need to
    /// keep track of the amount of space allocated inside the let expression
    /// and free it afterwards.
    pub fn binding(
        s: &mut State,
        vars: &[(String, Expr)],
        body: &[Expr],
    ) -> ASM {
        let mut asm = ASM(vec![]);

        s.enter();

        for (name, expr) in vars {
            asm += eval(s, expr) + x86::save(RAX, s.si);
            s.set(name, s.si);
        }

        for b in body {
            asm += eval(s, &b);
        }

        s.leave();
        asm
    }

    /// Emit code for a conditional expression
    pub fn cond(
        s: &mut State,
        p: &Expr,
        then: &Expr,
        alt: &Option<Box<Expr>>,
    ) -> ASM {
        let exit_label = s.gen_label();
        let alt_label = s.gen_label();

        // A conditional without an explicit alternate should evaluate to '()
        let t = match alt {
            None => &Expr::Nil,
            Some(t) => t,
        };

        eval(s, p)
            + x86::cmp(RAX, immediate::FALSE)
            + x86::je(&alt_label)
            + eval(s, then)
            + x86::jmp(&exit_label)
            + x86::label(&alt_label)
            + eval(s, t)
            + x86::label(&exit_label)
    }

    /// Evaluate an expression into RAX
    ///
    /// If the expression fits in a machine word, immediately return with the
    /// immediate repr, recurse for anything else till the base case.
    ///
    // TODO: eval should dispatch based on first atom alone, not necessarily
    // care about arity here. `let` and other variadic syntax forms won't fit
    // into any specific branch here.

    #[allow(clippy::redundant_pattern)]
    pub fn eval(s: &mut State, prog: &Expr) -> ASM {
        match prog {
            Identifier(i) => match s.get(i) {
                Some(i) => x86::load(RAX, i).into(),
                None => panic!("Undefined variable {}", i),
            },

            // Find the symbol index and return and reference in RAX
            Str(data) => strings::eval(&s, &data),

            Let { bindings, body } => binding(s, bindings, body),

            Cond { pred, then, alt } => cond(s, pred, then, alt),

            List(list) => match list.as_slice() {
                // User defined functions
                [Identifier(f), args @ ..] if s.functions.contains(f) => {
                    lambda::call(s, f, &Expressions(args.to_vec()))
                }

                [Identifier(i), arg] => match &i[..] {
                    "inc" => primitives::inc(s, arg),
                    "dec" => primitives::dec(s, arg),
                    "null?" => primitives::nullp(s, arg),
                    "zero?" => primitives::zerop(s, arg),
                    "not" => primitives::not(s, arg),
                    "boolean?" => primitives::booleanp(s, arg),
                    "char?" => primitives::charp(s, arg),
                    "fixnum?" => primitives::fixnump(s, arg),
                    "pair?" => primitives::pairp(s, arg),
                    "string?" => primitives::stringp(s, arg),
                    "car" => primitives::car(s, arg),
                    "cdr" => primitives::cdr(s, arg),
                    "make-string" => primitives::string::make(s, arg),
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

                _ => panic!("Unknown expression: `{}`", prog),
            },

            _ => x86::mov(RAX, immediate::to(&prog)).into(),
        }
    }

    /// Top level interface to the emit module
    pub fn program(prog: Expressions) -> String {
        let mut s: State = Default::default();

        strings::lift(&mut s, &prog);

        let (codes, prog) = lambda::lift(&mut s, &prog);

        let mut gen = x86::prelude()
            + x86::func(&x86::init())
            + x86::enter()
            + x86::init_heap();

        for b in &prog.0 {
            gen += eval(&mut s, &b);
        }

        gen += x86::leave();
        gen += strings::inline(&s);
        gen += lambda::code(&mut s, codes);

        gen.to_string()
    }
}
