#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]

include!("bindings.rs");

use std::os::raw::c_uint;
use std::io::Write;

enum PrintState {
    OUT,
    IN,
    DISPLY
}

fn print_ptr_rec(mut port: std::io::Stdout, x: ptr, state: PrintState) {
    if  (x & fx_mask) == fx_tag {
        write!(port, "{}", (x as c_uint) >> fx_shift);
    } else {
        write!(port, "TODO");
    }
}
fn print_ptr(x: ptr) {
    print_ptr_rec(std::io::stdout(), x, PrintState::OUT);
    println!("");
}

fn main() {
    print_ptr(4);
}
