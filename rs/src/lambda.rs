//! Scheme functions
//!
//! Scheme lambdas are heavily overloaded and this is one of the most
//! sophisticated parts of this compiler.
//!
//! A scheme lambda creates a callable function and a heap allocated closure
//! object that captures the free variables. This module also implements a
//! calling convention - which is a set of rules agreed by the caller of a
//! function and its definition regarding how the arguments are passed in and
//! how a result is returned.
//!
//! Closure conversion aims to break down scheme lambdas into something simpler
//! for code generation and is a well known technique in several functional
//! compilers. All lambdas are lifted to top level with a unique name and an
//! explicit closure object is passed as the first argument which captures the
//! environment in which the function was defined.
//!
//! See [Closure conversion: How to compile lambda][cc] for a detailed
//! explanation.
//!
//! [cc]: http://matt.might.net/articles/closure-conversion/
//!
//! The paper uses a calling convention that passes all arguments in stack. This
//! is harder to implement than the default calling calling convention used by
//! GCC on x86-64 - System V AMD64 ABI, in which arguments are passed in the
//! registers RDI, RSI, RDX, RCX, R8, R9 and the return value is passed back in
//! RAX.
//!
//! ⚠ This module implements the stack version for now, but must be migrated to
//! SysV at some point.
use crate::{
    compiler::{emit::eval, state::State},
    core::{
        Expr::{self, *},
        Expressions,
    },
    x86::{self, Register::*, ASM, WORDSIZE},
};

use std::convert::TryInto;

/// Scan through the source and lift lambdas into top level.
//
// TODO:
// 1. Ensure labels are unique
//
pub fn lift(s: &mut State, prog: &Expressions) -> (Expressions, Expressions) {
    let mut codes: Expressions = Default::default();
    let mut lifted: Expressions = Default::default();

    for expr in &prog.0 {
        let (c, e) = lift1(s, &expr);

        codes.0.extend(c);
        lifted.0.push(e);
    }

    (codes, lifted)
}

/// Lift a single expression to top level
fn lift1(s: &mut State, prog: &Expr) -> (Vec<Expr>, Expr) {
    type T = Vec<(String, Expr)>;
    match prog {
        // Lift lambda bindings while and give it a name
        //
        // 1. If the binding is a λ, remove it and add to labels
        Let { bindings, body } => {
            let (functions, rest): (T, T) = bindings
                .clone()
                .into_iter()
                .partition(|(_name, value)| match &value {
                    Lambda { .. } => true,
                    _ => false,
                });

            let codes = functions
                .iter()
                .map(|(name, f)| {
                    s.functions.insert(name.to_string());
                    match &f {
                        // Attach the bound name to lambda
                        Lambda { formals, free, body, .. } => Lambda {
                            name: Some(name.to_string()),
                            formals: formals.to_vec(),
                            free: free.to_vec(),
                            body: body.to_vec(),
                        },
                        _ => unreachable!("non λ in a list of lambdas"),
                    }
                })
                .collect();

            let expr = Let { bindings: rest, body: body.to_vec() };

            (codes, expr)
        }

        // A literal lambda must be in an inline calling position
        Lambda { .. } => unimplemented!("inline λ"),

        _ => (vec![], prog.clone()),
    }
}

/// Function body for the simplest C style functions
///
/// A lot of required sanity and safety checks are missing.
///
/// The calling convention expected by the function is kind of odd and needs
/// to be standardized. Arguments are pushed to stack in order (unlike
/// cdecl, which pushes in reverse order). System V AMD64 ABI would be
/// perfect since all args are passed in registers and is a lot cleaner and
/// is already used for `init()`
///
/// The caller of the function emits arguments at `RSP - 24`, then `RSP -
/// 32` etc. The function preamble effectively decrements the base pointer
/// by `0x10` such that the such that the first argument can be accessed at
/// `RBP - 8`, the next one at `RBP - 16` etc.
pub fn code(s: &mut State, codes: Expressions) -> ASM {
    let mut asm = ASM(vec![]);

    for code in &codes.0 {
        match code {
            Lambda { name: Some(name), formals, body, .. } => {
                asm += x86::func(name);

                // Start a new lexical environment for the function, add the formal
                // arguments and leave when it is evaluated. The first argument is
                // available at `RSP - 8`, next at `RBP - 16` etc.
                //
                // TODO: `alloc()` and `dealloc()` doesn't understand `enter()` and
                // `leave()`, so there is a fair bit of duplication here.
                s.enter();

                for (i, arg) in formals.iter().enumerate() {
                    let i: i64 = i.try_into().unwrap();
                    s.set(&arg, -(i + 1) * WORDSIZE);
                }

                for b in body {
                    asm += x86::enter();
                    asm += eval(s, b);
                    asm += x86::leave()
                }

                s.leave()
            }
            _ => unreachable!("emit λ for `{}`", code),
        }
    }
    asm
}

/// Emit code for a function application. See `code` for details.
pub fn call(s: &mut State, name: &str, args: &Expressions) -> ASM {
    // Evaluate and push the arguments into stack; 2 words below SI. See
    // `code` docs for a detailed description of how this works.
    //
    // si  832 -> ...
    //     816 -> ...
    //     808 -> arg 1
    //     800 -> arg 2
    let mut asm = ASM(vec![]);
    for (i, arg) in args.0.iter().enumerate() {
        let i: i64 = i.try_into().unwrap();
        asm += eval(s, arg);
        asm += x86::save(RAX, s.si - ((i + 2) * WORDSIZE));
    }

    // Extend stack to hold the current local variables before creating a
    // new frame for the function call. `si` is the next available empty
    // slot, `(+ si wordsize)` is the current usage. Add this to `RSP` to
    // reserve this space before the function gets called. Not doing this
    // will result in the called function to override this space with its
    // local variables and corrupt the stack.
    asm += x86::add(RSP, s.si + WORDSIZE);

    asm += x86::call(name);

    // NOTE: This is one of those big aha moments.
    //
    // There is no need to reclaim space used for function arguments because
    // the memory would just get overridden by next variable allocation.
    // This makes keeping track of that stack index unnecessary and life so
    // much simpler.

    asm += x86::add(RSP, -(s.si + WORDSIZE));

    asm
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser;

    #[test]
    fn simple() {
        let prog = r"(let ((id (lambda (x) x))) (id 42))";
        let mut s: State = Default::default();

        let expr = match parser::parse(prog) {
            Ok(r) => r,
            Err(e) => panic!(e),
        };

        let (codes, e) = lift(&mut s, &expr);

        assert_eq!(
            codes.0,
            vec![Lambda {
                name: Some("id".into()),
                formals: vec!["x".into()],
                free: vec![],
                body: vec![Identifier("x".into())],
            }]
        );

        assert_eq!(
            e.0[0],
            Let {
                bindings: vec![],
                body: vec![List(vec![Identifier("id".into()), Number(42)])]
            }
        );
    }

    // #[test]
    // fn recursive() {
    //     let sample = r"(letrec ((e (lambda (x) (if (zero? x) #t (o (dec x)))))
    //                             (o (lambda (x) (if (zero? x) #f (e (dec x))))))
    //                      (e 25)))";
    //
    //     let lifted = r"(labels ((o (code (x) (e) (if (zero? x) #f (e (dec x)))))
    //                             (e (code (x) (o) (if (zero? x) #t (o (dec x))))))
    //                      (labelcall e 25)))";
    //
    //     // super::lift(s: &mut State, prog: &Expr) {
    //     assert_eq!(sample, lifted);
    // }
}
