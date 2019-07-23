//! Scheme primitives implemented directly in the compiler
//!
//! Several scheme functions like `(add ...` are implemented by the compiler in
//! assembly rather than in scheme. All of them live in this module.

use crate::{
    compiler::emit::{self, eval},
    compiler::state::State,
    core::*,
    immediate::{self, *},
    x86::{Ins::*, Operand::*, Register::*, *},
};

// Unary Primitives

/// Increment number by 1
pub fn inc(s: &mut State, x: &AST) -> ASM {
    emit::eval(s, x) + Add { r: RAX, v: Const(immediate::n(1)) }
}

/// Decrement by 1
pub fn dec(s: &mut State, x: &AST) -> ASM {
    emit::eval(s, x) + Sub { r: RAX, v: Const(immediate::n(1)) }
}

/// Is the expression a fixnum?
///
/// # Examples
///
/// ```scheme
/// (fixnum? 42) => #t
/// (fixnum? "hello") => #f
/// ```
pub fn fixnump(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(Reg(RAX), Const(immediate::NUM), "sete")
}

/// Is the expression a boolean?
pub fn booleanp(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(Reg(RAX), Const(immediate::BOOL), "sete")
}

/// Is the expression a char?
pub fn charp(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(Reg(RAX), Const(immediate::CHAR), "sete")
}

/// Is the expression null?
pub fn nullp(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr) + compare(Reg(RAX), Const(immediate::NIL), "sete")
}

/// Is the expression a pair?
pub fn pairp(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(Reg(RAX), Const(immediate::PAIR), "sete")
}

/// Is the expression zero?
pub fn zerop(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr) + compare(Reg(RAX), Const(immediate::NUM), "sete")
}

/// Logical not
pub fn not(s: &mut State, expr: &AST) -> ASM {
    emit::eval(s, expr) + compare(Reg(RAX), Const(immediate::FALSE), "sete")
}

// Binary Primitives

/// Evaluate arguments and store the first argument in stack and second in `RAX`
fn binop(s: &mut State, x: &AST, y: &AST) -> ASM {
    emit::eval(s, x) + Save { r: RAX, si: s.si } + emit::eval(s, y)
}

/// Add `x` and `y` and move result to register RAX
pub fn plus(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, &x, &y) + Add { r: RAX, v: Stack(s.si) }
}

/// Subtract `x` from `y` and move result to register RAX
//
// `sub` Subtracts the 2nd op from the first and stores the result in the
// 1st. This is pretty inefficient to update result in stack and load it
// back. Reverse the order and fix it up.
pub fn minus(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, &x, &y)
        + Sub { r: RAX, v: Stack(s.si) }
        + Load { r: RAX, si: s.si }
}

/// Multiply `x` and `y` and move result to register RAX
// The destination operand is of `mul` is an implied operand located in
// register AX. GCC throws `Error: ambiguous operand size for `mul'` without
// size quantifier
pub fn mul(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, &x, &y)
        + Sar { r: RAX, v: immediate::SHIFT }
        + Mul { v: Stack(s.si) }
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
fn div(s: &mut State, x: &AST, y: &AST) -> ASM {
    emit::eval(s, y)
        + Ins::Sar { r: RAX, v: immediate::SHIFT }
        + Ins::Mov { to: Reg(RCX), from: Reg(RAX) }
        + emit::eval(s, x)
        + Ins::Sar { r: RAX, v: immediate::SHIFT }
        + Ins::Mov { to: Reg(RDX), from: Const(0) }
        + Ins::from("    cqo \n")
        + Ins::from("    idiv rcx \n")
}

pub fn quotient(s: &mut State, x: &AST, y: &AST) -> ASM {
    div(s, x, y) + Sal { r: RAX, v: immediate::SHIFT }
}

pub fn remainder(s: &mut State, x: &AST, y: &AST) -> ASM {
    div(s, x, y)
        + Mov { to: Reg(RAX), from: Reg(RDX) }
        + Sal { r: RAX, v: immediate::SHIFT }
}

/// Compares the first operand with the second with `SETcc`
// See `Ins::Cmp` to see how the compare instruction works.
//
// `SETcc` sets the destination operand to 0 or 1 depending on the settings of
// the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
//
// `MOVZX` copies the contents of the source operand (register or memory
// location) to the destination operand (register) and zero extends the value.
fn compare(a: Operand, b: Operand, setcc: &str) -> ASM {
    Cmp { a, b }
        + Ins::from(format!("    {} al\n", setcc))
        + Ins::from("    movzx rax, al\n")
        + Ins::from(format!("    sal al, {}\n", immediate::SHIFT))
        + Ins::from(format!("    or al, {}\n", immediate::BOOL))
}

/// Logical eq
pub fn eq(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, x, y) + compare(Stack(s.si), Reg(RAX), "sete")
}

/// Logical <
pub fn lt(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, x, y) + compare(Stack(s.si), Reg(RAX), "setl")
}

/// Logical >
pub fn gt(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, x, y) + compare(Stack(s.si), Reg(RAX), "setg")
}

/// Logical <=
pub fn lte(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, x, y) + compare(Stack(s.si), Reg(RAX), "setle")
}

/// Logical >=
pub fn gte(s: &mut State, x: &AST, y: &AST) -> ASM {
    binop(s, x, y) + compare(Stack(s.si), Reg(RAX), "setge")
}

// Allocation primitives

/// Allocate a pair on heap
pub fn cons(s: &mut State, x: &AST, y: &AST) -> ASM {
    // 1. Evaluate the first argument and push to stack
    // 2. Evaluate second argument
    // 3. Write second arg to [heap + 8]
    // 4. Fetch first argument back to RAX
    // 5. Write first arg from RAX to [heap + 0]
    // 6. Deallocate a word used for first arg
    let bp = s.si;
    let scratch = s.alloc();
    let ctx = emit::eval(s, x)
        + Save { r: RAX, si: scratch }
        + emit::eval(s, y)
        + Mov { to: Heap(8), from: Reg(RAX) }
        + Mov { to: Reg(RAX), from: Stack(scratch) }
        + Mov { to: Heap(0), from: Reg(RAX) }
        + Mov { to: Reg(RAX), from: Reg(RSI) }
        + Add { r: RSI, v: Operand::Const(WORDSIZE * 2) }
        + Or { r: RAX, v: Operand::Const(PAIR) };

    s.dealloc(1);
    assert!(s.si == bp, "Stack deallocated; expected {}, found {} ", bp, s.si);
    ctx
}

/// First half of a pair
// Subtracting the tag from the heap pointer gets us back the real address.
pub fn car(s: &mut State, pair: &AST) -> ASM {
    // Assert destination is really a pair ?
    eval(s, pair)
        + Ins::from(format!("    mov rax, [rax - {}]    # (car ..) \n", PAIR))
}

/// Second half of a pair
// Offset for cdr is (address - tag + 8) = 5
pub fn cdr(s: &mut State, pair: &AST) -> ASM {
    // Assert destination is really a pair ?
    eval(s, pair)
        + Ins::from(format!("    mov rax, [rax + {}]    # (cdr ...) \n", 5))
}

// String primitives
pub mod string {
    use super::*;
    use crate::compiler::string;

    pub fn make(s: &mut State, arg: &AST) -> ASM {
        match arg {
            AST::Number(n) => string::make(s, *n),
            _ => panic!("`make-string` expected number, got {:?}", arg),
        }
    }
}
