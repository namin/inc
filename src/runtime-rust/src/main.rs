#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

#![allow(unused_must_use)]
#![allow(unused_variables)]

include!("bindings.rs");

use std::os::raw::c_char;
use std::io::Write;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::IntoRawFd;
use std::io::Read;

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
                *((p.buf.as_ptr()).add(i as usize)),
                PrintState::OUT)
        }}
        write!(port, ")");
    } else if (x & obj_mask) == string_tag {
        if state == PrintState::OUT { write!(port, "\""); }
        unsafe {
        let p = &*((x-string_tag) as *const string);
        let n = (p.length as i32) >> fx_shift;
        for i in 0..n {
            let c = std::char::from_u32(*((p.buf.as_ptr()).add(i as usize)) as u32)
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
    println!();
}

fn eprint_ptr(msg: ptr, state: PrintState) {
    print_ptr_rec(&mut std::io::stderr(), msg, state);
}

#[no_mangle]
pub extern "C" fn ik_log(msg: ptr) -> ptr {
    eprint!("log: ");
    eprint_ptr(msg, PrintState::IN);
    eprintln!();
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
    eprintln!();
    std::process::exit(0);
}

fn unshift(x: ptr) -> i32 {
    (x as i32) >> fx_shift
}
fn shift(x: i32) -> ptr {
    (x << fx_shift) as u32
}
fn ptr_string_to_str(x: ptr) -> String {
    unsafe {
        let p = &*((x-string_tag) as *const string);
        let n = unshift(p.length) as usize;
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            v.push(*(p.buf.as_ptr().add(i as usize)) as u8);
        }
        std::ffi::CString::from_vec_unchecked(v).into_string().expect("string")
    }
}
#[no_mangle]
pub extern "C" fn s_write(fd: ptr, str: ptr, len: ptr) -> ptr {
    let s = ptr_string_to_str(str);
    let len = unshift(len) as usize;
    let s: String = s.chars().take(len).collect();
    let ufd = unshift(fd) as i32;
    let mut w = unsafe { std::fs::File::from_raw_fd(ufd) };
    write!(w, "{}", s);
    w.flush();
    w.into_raw_fd();
    shift(s.len() as i32)
}
#[no_mangle]
pub extern "C" fn s_open_write(fname: ptr) -> ptr {
    let s_fname = ptr_string_to_str(fname);
    let port = std::fs::File::create(s_fname).expect("created");
    let fd = port.into_raw_fd();
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
    let s_fname = ptr_string_to_str(fname);
    let port = std::fs::File::open(s_fname).expect("opened");
    let fd = port.into_raw_fd();
    shift(fd)
}
#[no_mangle]
pub extern "C" fn s_read_char(fd: ptr) -> ptr {
    let ufd = unshift(fd) as i32;
    let mut w = unsafe { std::fs::File::from_raw_fd(ufd) };
    let mut b = [0; 1];
    w.read_exact(&mut b);
    let v = if b[0] == 0 { eof_obj }
        else { ((b[0] as u32) << char_shift) | char_tag };
    w.into_raw_fd();
    v
}
#[no_mangle]
pub extern "C" fn s_close(fd: ptr) -> ptr {
    let ufd = unshift(fd) as i32;
    let w = unsafe { std::fs::File::from_raw_fd(ufd) };
    std::mem::drop(w);
    shift(0)
}

fn gc_align(n: usize) -> usize {
  let cell = 1 << obj_shift;
  (((n + cell - 1)/cell) * cell) as usize
}

fn gc_ptr_object(x: ptr) -> bool {
    let tag = x & obj_mask;
    match tag {
        pair_tag |
        vector_tag |
        symbol_tag |
        string_tag |
        closure_tag => true,
        _ => false
    }
}
fn gc_size(x: ptr) -> usize {
    let tag = x & obj_mask;
    let p = x-tag;
    match tag {
        pair_tag => 2 << word_shift,
        vector_tag => {
            let n = shift(unsafe{(&*(p as *const vector)).length} as i32) + 1;
            (n as usize) << word_shift
        },
        symbol_tag | string_tag =>
            (shift(unsafe{(&*(p as *const string)).length} as i32) + word_size) as usize,
        closure_tag => {
            let n = shift(unsafe{(&*(p as *const closure)).length} as i32) + 2;
            (n as usize) << word_shift
        },
        _ => 0
    }
}


static mut gc_new_heap_base: *mut c_char = 0 as *mut c_char;
static mut gc_new_heap_top: *mut c_char = 0 as *mut c_char;
static mut gc_next: *mut c_char = 0 as *mut c_char;
static mut gc_queue: *mut ptr = 0 as *mut ptr;

fn gc_get_forward_pointer(p: *mut c_char) -> *mut c_char {
    let x = unsafe{*p as ptr};
    if x != gc_forward_mark { return 0 as *mut c_char; }
    let q = unsafe{*((p as *mut ptr).add(1)) as *mut c_char};
    q
}

fn gc_set_forward_pointer(p: *mut c_char, q: *mut c_char) {
    unsafe {
        *p = gc_forward_mark as c_char;
        *((p as *mut ptr).add(1)) = q as ptr;
    }
}

fn gc_forward(x: ptr) -> ptr {
    if !gc_ptr_object(x) {
        return x;
    }
    
    let tag = x & obj_mask;
    let p = (x-tag) as *mut c_char;
    let mut q = gc_get_forward_pointer(p);
    if q != (0 as *mut c_char) {
        return (q as ptr) | tag;
    }

    unsafe {
    q = gc_next;
    let n = gc_size(x);
    for i in 0..n {
        *gc_next.add(i) = *p.add(i);
    }
    gc_next = gc_next.add(gc_align(n));
    gc_set_forward_pointer(p, q);
    let f = ((q as ptr) | tag) as ptr;
    *gc_queue = f;
    gc_queue = gc_queue.add(1);
    f
    }
}
    

fn gc(mem: *mut memory, stack: *mut c_char) {
    unsafe {
    gc_new_heap_base = (*mem).heap_base_alt as *mut c_char;
    gc_new_heap_top = (*mem).heap_top_alt as *mut c_char;

    gc_next = (*mem).heap_base_alt as *mut c_char;
    gc_queue = (*mem).scratch_base as *mut ptr;
    }

    let scan = unsafe{gc_queue};
    let mut root = unsafe{*mem}.global_base as *mut ptr;
    while unsafe { (root as *mut c_char) < (*mem).global_next } {
        unsafe {
            *root = gc_forward(*root);
            root = root.add(1);
        }
    }

    root = unsafe{*mem}.stack_base as *mut ptr;
    unsafe {
    root.offset(-1);
    root.offset(-1);
        while root > (stack as *mut ptr) {
            if *root == return_addr {
                root.offset(-1);
            } else {
                *root = gc_forward(*root);
            }
        }
        root.offset(-1);
    }

    unsafe {
        (*mem).edi = gc_forward((*mem).edi);
    }

    while scan < unsafe{gc_queue} {
        let x = unsafe { *(scan.add(1)) };
        let tag = x & obj_mask;

        if tag == pair_tag {
            unsafe {
                let p = (x-tag) as *mut cell;
                (*p).car = gc_forward((*p).car);
                (*p).cdr = gc_forward((*p).cdr);
            }
        } else if tag == vector_tag {
            unsafe {
                let p = (x-tag) as *mut vector;
                let len = unshift((*p).length) as usize;
                let b = (*p).buf.as_mut_ptr();
                for i in 0..len {
                    *(b.add(i)) = gc_forward(*(b.add(i)));
                }
            }
        } else if tag == closure_tag {
            unsafe {
                let p = (x-tag) as *mut closure;
                let len = unshift((*p).length) as usize;
                let b = (*p).fvs.as_mut_ptr();
                for i in 0..len {
                    *(b.add(i)) = gc_forward(*(b.add(i)));
                }
            }
        }
    }

    unsafe {
        (*mem).heap_next = gc_next;
        (*mem).heap_base_alt = (*mem).heap_base;
        (*mem).heap_top_alt = (*mem).heap_top;
        (*mem).heap_base = gc_new_heap_base;
        (*mem).heap_top = gc_new_heap_top;
    }
}

#[no_mangle]
pub extern "C" fn heap_alloc(mem: *mut memory, stack: *mut c_char, size: usize) -> *mut c_char {
    let mut heap_next = unsafe {*mem}.heap_next;
    let mut heap_new = unsafe { heap_next.add(size) };
    if unsafe { heap_new >= (*mem).heap_top } {
        gc(mem, stack);
        heap_next = unsafe{*mem}.heap_next;
        heap_new = unsafe { heap_next.add(size) };
        if unsafe { heap_new >= (*mem).heap_top } {
            eprintln!("Exception: overflow");
            std::process::exit(0);
        }
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
    let stack_base = unsafe { stack_top.add(stack_size) };

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

    let heap_top = unsafe { heap.add(heap_size/2) };
    let mut mem = memory {
        heap_next : heap,
        global_next : global,
        heap_base : heap,
        heap_top : heap_top,
        heap_base_alt : heap_top,
        heap_top_alt : unsafe { heap.add(heap_size) },
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
