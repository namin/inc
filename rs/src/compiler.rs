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
    /// State should also implement some form of register allocation.
    pub struct State {
        pub si: i64,
        pub asm: ASM,
        env: Env,
    }

    impl Default for State {
        fn default() -> Self {
            State { si: -WORDSIZE, asm: ASM(vec![]), env: new() }
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

        /// Allocate a word on the stack
        fn alloc(&mut self) {
            self.si -= WORDSIZE;
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

/// Emit machine code for inc AST.
///
/// This module implements bulk of the compiler and is a good place to start
/// reading code. Platform specific code is annotated with `cfg(target_os)` for
/// both linux and mac. This module implements code gen specific to inc and
/// anything generic goes into `x86` module.
pub mod emit {
    use crate::{
        compiler::state::State,
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

    pub fn binding(
        s: &mut State,
        bindings: &[(String, AST)],
        body: &[AST],
    ) -> ASM {
        let mut ctx = String::new();

        s.enter();

        for (name, expr) in bindings.iter() {
            let x = eval(s, expr) + Save { r: RAX, si: s.si };
            s.set(name, s.si);
            ctx.push_str(&x.to_string());
        }

        for b in body.iter() {
            let x = eval(s, &b);
            ctx.push_str(&x.to_string());
        }

        s.leave();
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
    pub fn eval(s: &mut State, prog: &AST) -> ASM {
        match prog {
            Identifier(i) => match s.get(i) {
                Some(i) => Ins::Load { r: RAX, si: i }.into(),
                None => panic!("Undefined variable {}", i),
            },

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
                    "char?" => primitives::charp(s, arg),
                    n => panic!("Unknown unary primitive: {}", n),
                },

                [Identifier(name), x, y] => match &name[..] {
                    "+" => primitives::plus(s, x, y),
                    "-" => primitives::minus(s, x, y),
                    "*" => primitives::mul(s, x, y),
                    "/" => primitives::quotient(s, x, y),
                    "%" => primitives::remainder(s, x, y),

                    n => panic!("Unknown binary primitive: {}", n),
                },

                l => panic!("Unknown expression: {:?}", l),
            },

            _ => Mov { to: Reg(RAX), from: Const(immediate::to(&prog)) }.into(),
        }
    }

    /// Top level interface to the emit module
    pub fn program(prog: &AST) -> String {
        let mut s: State = Default::default();
        let gen =
            x86::function_header("init") + Enter + eval(&mut s, prog) + Leave;

        gen.to_string()
    }
}
