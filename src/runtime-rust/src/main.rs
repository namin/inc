#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_must_use)]

include!("bindings.rs");

//use std::os::raw::c_uint;
//use std::os::raw::c_int;
use std::os::raw::c_char;
use std::io::Write;

#[derive(PartialEq)]
enum PrintState {
    OUT,
    IN,
    DISPLAY
}

fn print_ptr_rec(mut port: std::io::Stdout, p: ptr, state: PrintState) {
    let x = p as u32;
    if  (x & fx_mask) == fx_tag {
        write!(port, "{}", (x as i32) >> fx_shift);
    } else if x == bool_f {
        write!(port, "#f");
    } else if x == bool_t {
        write!(port, "#t");
    } else if x == list_nil {
        write!(port, "()");
    } else if x == eof_obj {
        write!(port, "#!eof");
    } else if (x & char_mask) == char_tag {
        let c = std::char::from_u32(x >> char_shift).
            expect("a char");
        if state == PrintState::DISPLAY {
            write!(port, "{}", c);
        } else {
            if      c == '\t' { write!(port, "#\\tab"); }
            else if c == '\n' { write!(port, "#\\newline"); }
            else if c == '\r' { write!(port, "#\\return"); }
            else if c == ' '  { write!(port, "#\\space"); }
            else              { write!(port, "#\\{}", c); }
        }
    } else {
        write!(port, "TODO");
    }
}
fn print_ptr(x: ptr) {
    print_ptr_rec(std::io::stdout(), x, PrintState::OUT);
    println!("");
}

fn allocate_protected_space(size: usize) -> *mut c_char {
    let mut v = Vec::with_capacity(size);
    let ptr = v.as_mut_ptr();
    std::mem::forget(v);
    ptr
}

fn deallocate_protected_space(p: *mut c_char, size: usize) {
    unsafe { std::mem::drop(Vec::from_raw_parts(p, 0, size)) };
}

fn main() {
    let stack_size = 16 * 4096;
    let heap_size = 4 * 16 * 4096;
    let global_size = 16 * 4096;
    let scratch_size = 16 * 4096;

    let stack_top = allocate_protected_space(stack_size);
    let stack_base = unsafe { stack_top.offset(stack_size as isize) };

    let heap = allocate_protected_space(heap_size);
    let global = allocate_protected_space(global_size);
    let scratch = allocate_protected_space(scratch_size);

    let uninit = 0 as (*mut std::os::raw::c_void);

    let mut ctxt = context {
        eax : uninit,
        ebx : uninit,
        ecx : uninit,
        edx : uninit,
        esi : uninit,
        edi : uninit,
        ebp : uninit,
        esp : uninit,
    };

    let heap_top = unsafe { heap.offset((heap_size as isize)/2) };
    let mut mem = memory {
        heap_next : heap,
        global_next : global,
        heap_base : heap,
        heap_top : heap_top,
        heap_base_alt : heap_top,
        heap_top_alt : unsafe { heap.offset(heap_size as isize) },
        global_base : global,
        stack_base : stack_base,
        scratch_base : scratch,
        edi : 0
    };

    print_ptr(unsafe {
        scheme_entry(&mut ctxt, stack_base, &mut mem)
    });

    deallocate_protected_space(stack_top, stack_size);
    deallocate_protected_space(heap, stack_size);
}
