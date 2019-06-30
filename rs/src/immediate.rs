//! Runtime representation of typed scheme values
//!
//! Immediate values (values that can be fit in one machine word) are tagged for
//! distinguising them from heap allocated pointers. The last 3 bits effectively
//! serve as the runtime type of the value. Always using 3 bits is a simpler
//! approach than the multi bit technique the paper uses. This is a very
//! efficient and low overhead technique at the cost of losing precision -
//! completely acceptable for types like characters and booleans but having to
//! live with 61bit numerics instead of native 64 and some overhead for
//! operations like multiplication & division.
//!
//! See the paper for details. See tests for examples.

use crate::core::*;

pub const NUM: i64 = 0;
pub const BOOL: i64 = 1;
pub const CHAR: i64 = 2;
pub const PAIR: i64 = 3;
pub const NIL: i64 = 4;
pub const STR: i64 = 5;
pub const HEAP: i64 = 7;

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
        AST::Identifier(i) => {
            unimplemented!("immediate repr is undefined for identifier {}", i)
        }
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
