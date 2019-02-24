#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_must_use)]

include!("bindings.rs");

use std::os::raw::c_char;
use std::io::Write;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::IntoRawFd;

extern crate libc;

#[derive(PartialEq)]
enum PrintState {
    OUT,
    IN,
    DISPLAY
}

fn print_ptr_rec<W: Write>(port: &mut W, p: ptr, state: PrintState) {
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
    } else if (x & obj_mask) == pair_tag {
        if state == PrintState::OUT { write!(port, "("); }
        let cell = unsafe { *((x-pair_tag) as *const cell) };
        let car = cell.car;
        print_ptr_rec(port, car, PrintState::OUT);
        let cdr = cell.cdr;
        if cdr != list_nil {
            if (cdr & obj_mask) != pair_tag {
                write!(port, " . ");
                print_ptr_rec(port, cdr, PrintState::OUT);
            } else {
                write!(port, " ");
                print_ptr_rec(port, cdr, PrintState::IN);
            }
        }
        if state == PrintState::OUT { write!(port, ")"); }
    } else if (x & obj_mask) == vector_tag {
        write!(port, "#(");
        unsafe {
        let p = &*((x-vector_tag) as *const vector);
        let n = (p.length as i32) >> fx_shift;
        for i in 0..n {
            if i > 0 { write!(port, " "); }
            print_ptr_rec(port,
                *((p.buf.as_ptr()).offset(i as isize)),
                PrintState::OUT)
        }}
        write!(port, ")");
    } else if (x & obj_mask) == string_tag {
        if state == PrintState::OUT { write!(port, "\""); }
        unsafe {
        let p = &*((x-string_tag) as *const string);
        let n = (p.length as i32) >> fx_shift;
        for i in 0..n {
            let c = std::char::from_u32(*((p.buf.as_ptr()).offset(i as isize)) as u32)
                .expect("char");
            if c == '"'       { write!(port, "\\\""); }
            else if c == '\\' { write!(port, "\\\\"); }
            else              { write!(port, "{}", c); }
        }}
        if state == PrintState::OUT { write!(port, "\""); }
    } else if (x & obj_mask) == symbol_tag {
        print_ptr_rec(port, (x - symbol_tag) | string_tag, PrintState::IN);
    } else if (x & obj_mask) == closure_tag {
        write!(port, "#<procedure>");
    } else {
        write!(port, "#<unknown 0x{:x}>", x);
    }
}
fn print_ptr(x: ptr) {
    print_ptr_rec(&mut std::io::stdout(), x, PrintState::OUT);
    println!("");
}

fn eprint_ptr(msg: ptr, state: PrintState) {
    print_ptr_rec(&mut std::io::stderr(), msg, PrintState::IN);
}

#[no_mangle]
pub extern "C" fn ik_log(msg: ptr) -> ptr {
    eprint!("log: ");
    eprint_ptr(msg, PrintState::IN);
    eprintln!("");
    0
}
#[no_mangle]
pub extern "C" fn ik_error(x: ptr) {
    eprint!("Exception");
    if (x & obj_mask) == pair_tag {
        let cell = unsafe { *((x-pair_tag) as *const cell) };
        let caller = cell.car;
        let msg = cell.cdr;
        if caller != bool_f {
            eprint!(" in ");
            eprint_ptr(caller, PrintState::OUT);
        }
        eprint!(": ");
        eprint_ptr(msg, PrintState::IN);
    }
    eprintln!("");
    std::process::exit(0);
}

fn unshift(x: ptr) -> i32 {
    (x as i32) >> fx_shift
}
fn shift(x: i32) -> ptr {
    (x << fx_shift) as u32
}
fn string_data(x: ptr) -> *mut c_char {
    unsafe {
        let p = &*((x-string_tag) as *const string);
        p.buf.as_ptr() as *mut c_char
    }
}
fn cp_str_data(x: ptr, buf: *mut c_char, buf_length: u32) {
    unsafe {
        let p = &*((x-string_tag) as *const string);
        let n = unshift(p.length) as u32;
        let m = std::cmp::min(n, buf_length-1);
        for i in 0..m {
            *(buf.offset(i as isize)) = *(p.buf.as_ptr().offset(i as isize));
        }
        *(buf.offset(m as isize)) = 0;
    }
}
#[no_mangle]
pub extern "C" fn s_write(fd: ptr, str: ptr, len: ptr) -> ptr {
    let bytes = unsafe { libc::write(unshift(fd), string_data(str) as *const libc::c_void, unshift(len) as usize)};
    shift(bytes as i32)
}
#[no_mangle]
pub extern "C" fn s_open_write(fname: ptr) -> ptr {
    let mut c_fname: [c_char; 10] = [0 as c_char; 10];
    cp_str_data(fname, c_fname.as_mut_ptr(), 10);
    let fd = unsafe { libc::open(c_fname.as_mut_ptr(), libc::O_WRONLY | libc::O_CREAT | libc::O_TRUNC, 0640) };
    shift(fd)
}
#[no_mangle]
pub extern "C" fn s_fflush(fd: ptr) -> ptr {
    let ufd = unshift(fd) as i32;
    let mut w = unsafe { std::fs::File::from_raw_fd(ufd) };
    w.flush();
    w.into_raw_fd();
    0
}
#[no_mangle]
pub extern "C" fn scheme_write(fd: ptr, x: ptr, opt: ptr) -> ptr {
    let ufd = unshift(fd) as i32;
    let state = unshift(opt);
    let state =
        if state == 0 { PrintState::OUT }
    else if state == 1 { PrintState::IN }
    else { PrintState :: DISPLAY };
    let mut w = unsafe { std::fs::File::from_raw_fd(ufd) };
    print_ptr_rec(&mut w, x, state);
    w.flush();
    w.into_raw_fd();
    0
}
#[no_mangle]
pub extern "C" fn s_open_read(fname: ptr) -> ptr {
    let mut c_fname: [c_char; 10] = [0; 10];
    cp_str_data(fname, c_fname.as_mut_ptr(), 10);
    let fd = unsafe { libc::open(c_fname.as_mut_ptr(), libc::O_RDONLY) };
    shift(fd)
}
#[no_mangle]
pub extern "C" fn s_read_char(fd: ptr) -> ptr {
    let mut ca: [u32; 1] = [0;1];
    let ufd = unshift(fd);
    if unsafe { libc::read(ufd, ca.as_mut_ptr() as *mut libc::c_void, 1) } < 1 {
        eof_obj
    } else {
        (ca[0] << char_shift) | char_tag
    }
}
#[no_mangle]
pub extern "C" fn s_close(fd: ptr) -> ptr {
    let ufd = unshift(fd) as i32;
    let w = unsafe { std::fs::File::from_raw_fd(ufd) };
    std::mem::drop(w);
    shift(0)
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



    let r = unsafe {
        scheme_entry(&mut ctxt, stack_base, &mut mem)
    };
    print_ptr(r);

    deallocate_protected_space(stack_top, stack_size);
    deallocate_protected_space(heap, stack_size);
}
