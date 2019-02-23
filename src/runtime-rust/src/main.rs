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

#[derive(PartialEq)]
enum PrintState {
    OUT,
    IN,
    DISPLAY
}

fn print_ptr_rec(p: ptr, state: PrintState) {
    let x = p as u32;
    if  (x & fx_mask) == fx_tag {
        print!("{}", (x as i32) >> fx_shift);
    } else if x == bool_f {
        print!("#f");
    } else if x == bool_t {
        print!("#t");
    } else if x == list_nil {
        print!("()");
    } else if x == eof_obj {
        print!("#!eof");
    } else if (x & char_mask) == char_tag {
        let c = std::char::from_u32(x >> char_shift).
            expect("a char");
        if state == PrintState::DISPLAY {
            print!("{}", c);
        } else {
            if      c == '\t' { print!("#\\tab"); }
            else if c == '\n' { print!("#\\newline"); }
            else if c == '\r' { print!("#\\return"); }
            else if c == ' '  { print!("#\\space"); }
            else              { print!("#\\{}", c); }
        }
    } else if (x & obj_mask) == pair_tag {
        if state == PrintState::OUT { print!("("); }
        let cell = unsafe { *((x-pair_tag) as *mut cell) };
        let car = cell.car;
        print_ptr_rec(car, PrintState::OUT);
        let cdr = cell.cdr;
        if cdr != list_nil {
            if (cdr & obj_mask) != pair_tag {
                print!(".");
                print_ptr_rec(cdr, PrintState::OUT);
            } else {
                print!(" ");
                print_ptr_rec(cdr, PrintState::IN);
            }
        }
        if state == PrintState::OUT { print!(")"); }
    } else {
        print!("TODO");
    }
}
fn print_ptr(x: ptr) {
    print_ptr_rec(x, PrintState::OUT);
    println!("");
}

#[no_mangle]
pub extern "C" fn ik_log(msg: ptr) {
}
#[no_mangle]
pub extern "C" fn ik_error(x: ptr) {
}
#[no_mangle]
pub extern "C" fn s_write(fd: ptr, str: ptr, len: ptr) -> ptr {
    0
}
#[no_mangle]
pub extern "C" fn s_open_write(fname: ptr) -> ptr {
    0
}
#[no_mangle]
pub extern "C" fn s_fflush(fd: ptr) -> ptr {
    0
}
#[no_mangle]
pub extern "C" fn scheme_write(fd: ptr, x: ptr, opt: ptr) -> ptr {
    0
}
#[no_mangle]
pub extern "C" fn s_open_read(fname: ptr) -> ptr {
    0
}
#[no_mangle]
pub extern "C" fn s_read_char(fd: ptr) -> ptr {
    0
}
#[no_mangle]
pub extern "C" fn s_close(fd: ptr) -> ptr {
    0
}

#[no_mangle]
pub extern "C" fn heap_alloc(mem: *mut memory, stack: *mut c_char, size: usize) -> *mut c_char {
    let heap_next = unsafe { (*mem).heap_next };
    let heap_new = unsafe { heap_next.offset(size as isize) };
    if heap_new >= unsafe { (*mem).heap_top } {
        eprintln!("Exception: overflow");
        std::process::exit(0);
    }
    unsafe {(*mem).heap_next = heap_new};
    return heap_next;
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
