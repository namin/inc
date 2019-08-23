//! Scheme functions implemented within the compiler rather than the runtime.
use crate::{
    compiler::emit::{self, eval},
    compiler::state::State,
    core::*,
    immediate::{self, *},
    x86::{self, Register::*, *},
};

// Unary Primitives

/// Increment number by 1
pub fn inc(s: &mut State, x: &Expr) -> ASM {
    emit::eval(s, x) + x86::add(RAX.into(), immediate::n(1).into())
}

/// Decrement by 1
pub fn dec(s: &mut State, x: &Expr) -> ASM {
    emit::eval(s, x) + x86::sub(RAX.into(), immediate::n(1).into())
}

/// Is the expression a fixnum?
///
/// # Examples
///
/// ```scheme
/// (fixnum? 42) => #t
/// (fixnum? "hello") => #f
/// ```
pub fn fixnump(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(RAX.into(), immediate::NUM.into(), "sete")
}

/// Is the expression a boolean?
pub fn booleanp(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(RAX.into(), immediate::BOOL.into(), "sete")
}

/// Is the expression a char?
pub fn charp(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(RAX.into(), immediate::CHAR.into(), "sete")
}

/// Is the expression null?
pub fn nullp(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr) + compare(RAX.into(), immediate::NIL.into(), "sete")
}

/// Is the expression a pair?
pub fn pairp(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(RAX.into(), immediate::PAIR.into(), "sete")
}

/// Is the expression a string?
pub fn stringp(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr)
        + emit::mask()
        + compare(RAX.into(), immediate::STR.into(), "sete")
}

/// Is the expression zero?
pub fn zerop(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr) + compare(RAX.into(), immediate::NUM.into(), "sete")
}

/// Logical not
pub fn not(s: &mut State, expr: &Expr) -> ASM {
    emit::eval(s, expr) + compare(RAX.into(), immediate::FALSE.into(), "sete")
}

// Binary Primitives

/// Evaluate arguments and store the first argument in stack and second in `RAX`
fn binop(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    emit::eval(s, x) + x86::save(RAX.into(), s.si) + emit::eval(s, y)
}

/// Add `x` and `y` and move result to register RAX
pub fn plus(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, &x, &y) + x86::add(RAX.into(), Reference::from(RBP + s.si))
}

/// Subtract `x` from `y` and move result to register RAX
// `sub` subtracts the 2nd op from the first and stores the result in the 1st.
//
// Since binop evaluates x first and then y, this is a little clumsy. A
// temporary register RDI is used to swap the arguments before subtracting in
// right order.
//
//     x: [RBP - 8], y: RAX
//     y: RAX -> RDI
//     x: [RBP - 8] -> RAX
//     RAX  = RAX (x) - RDI (y)
pub fn minus(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, &x, &y)
        + x86::mov(RDI.into(), RAX.into())
        + x86::mov(RAX.into(), Reference::from(RBP + s.si))
        + x86::sub(RAX.into(), RDI.into())
}

/// Multiply `x` and `y` and move result to register RAX
// The destination operand is of `mul` is an implied operand located in register
// AX. GCC throws `Error: ambiguous operand size for `mul'` without size
// quantifier
pub fn mul(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, &x, &y)
        + x86::sar(RAX.into(), immediate::SHIFT.into())
        + x86::mul(Reference::from(RBP + s.si))
}

/// Divide `x` by `y` and move result to register RAX
// Division turned out to be much more trickier than I expected it to be.
// Unlike @namin's code, I'm using a shift arithmetic right (SAR) instead of
// shift logical right (SHR) and I don't know how the original examples worked
// at all for negative numbers. I also had to use the CQO instruction to
// Sign-Extend RAX which the 32 bit version is obviously not concerned with. I
// got the idea from GCC disassembly.
//
// Dividend is passed in RDX:RAX and IDIV instruction takes the divisor as the
// argument. the quotient is stored in RAX and the remainder in RDX.
fn div(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    emit::eval(s, y)
        + x86::sar(RAX.into(), immediate::SHIFT.into())
        + x86::mov(RCX.into(), RAX.into())
        + emit::eval(s, x)
        + x86::sar(RAX.into(), immediate::SHIFT.into())
        + x86::mov(RDX.into(), 0.into())
        + Ins::from("cqo")
        + Ins::from("idiv rcx")
}

/// Quotient after dividing `x` by `y`
pub fn quotient(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    div(s, x, y) + x86::sal(RAX.into(), immediate::SHIFT.into())
}

/// Remainder after dividing `x` by `y`
pub fn remainder(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    div(s, x, y)
        + x86::mov(RAX.into(), RDX.into())
        + x86::sal(RAX.into(), immediate::SHIFT.into())
}

/// Compares the first operand with the second with `SETcc`
// See `x86::Cmp` to see how the compare instruction works.
//
// `SETcc` sets the destination operand to 0 or 1 depending on the settings of
// the status flags (CF, SF, OF, ZF, and PF) in the EFLAGS register.
//
// `MOVZX` copies the contents of the source operand (register or memory
// location) to the destination operand (register) and zero extends the value.
fn compare(a: Reference, b: Reference, setcc: &str) -> ASM {
    x86::cmp(a, b)
        + Ins(format!("{} al", setcc))
        + Ins::from("movzx rax, al")
        + Ins(format!("sal al, {}", immediate::SHIFT))
        + Ins(format!("or al, {}", immediate::BOOL))
}

/// Logical eq
pub fn eq(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, x, y) + compare(Reference::from(RBP + s.si), RAX.into(), "sete")
}

/// Logical <
pub fn lt(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, x, y) + compare(Reference::from(RBP + s.si), RAX.into(), "setl")
}

/// Logical >
pub fn gt(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, x, y) + compare(Reference::from(RBP + s.si), RAX.into(), "setg")
}

/// Logical <=
pub fn lte(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, x, y) + compare(Reference::from(RBP + s.si), RAX.into(), "setle")
}

/// Logical >=
pub fn gte(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    binop(s, x, y) + compare(Reference::from(RBP + s.si), RAX.into(), "setge")
}

// Allocation primitives

/// Allocate a pair on heap
#[allow(clippy::identity_op)]
pub fn cons(s: &mut State, x: &Expr, y: &Expr) -> ASM {
    // 1. Evaluate the first argument and push to stack
    // 2. Evaluate second argument
    // 3. Write second arg to [heap + 8]
    // 4. Fetch first argument back to RAX
    // 5. Write first arg from RAX to [heap + 0]
    // 6. Deallocate a word used for first arg
    let bp = s.si;
    let scratch = s.alloc();
    let ctx = emit::eval(s, x)
        + x86::save(RAX.into(), scratch)
        + emit::eval(s, y)
        + x86::mov(Reference::from(RSI + 8), RAX.into())
        + x86::mov(RAX.into(), Reference::from(RBP + scratch))
        + x86::mov(Reference::from(RSI + 0), RAX.into())
        + x86::mov(RAX.into(), RSI.into())
        + x86::add(RSI.into(), Reference::from(WORDSIZE * 2))
        + x86::or(RAX.into(), PAIR.into());

    s.dealloc(1);
    assert!(s.si == bp, "Stack deallocated; expected {}, found {} ", bp, s.si);
    ctx
}

/// First half of a pair
// Subtracting the tag from the heap pointer gets us back the real address.
pub fn car(s: &mut State, pair: &Expr) -> ASM {
    // Assert destination is really a pair ?
    eval(s, pair) + Ins(format!("mov rax, [rax - {}]    # (car ..)", PAIR))
}

/// Second half of a pair
// Offset for cdr is (address - tag + 8) = 5
pub fn cdr(s: &mut State, pair: &Expr) -> ASM {
    // Assert destination is really a pair ?
    eval(s, pair) + Ins(format!("mov rax, [rax + {}]    # (cdr ...)", 5))
}

// String primitives
pub mod string {
    use super::*;
    use crate::strings;

    pub fn make(s: &mut State, arg: &Expr) -> ASM {
        match arg {
            Expr::Number(n) => strings::make(s, *n),
            _ => panic!("`make-string` expected number, got {:?}", arg),
        }
    }
}
