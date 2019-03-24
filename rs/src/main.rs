use std::io::{self, Read};

// Expects the program in stdin, writes assembly to stdout
fn main() -> std::io::Result<()> {
    let mut program = String::new();
    io::stdin()
        .read_to_string(&mut program)
        .expect("Expected a program in stdin");

    let i: i64 = program
        .trim_end()
        .parse()
        .expect("Failed to parse stdin to a valid program");

    println!("{}\n", compile_program(i));
    Ok(())
}

fn emit_label(label: &str) -> String {
    return format!("{}:\n", label);
}

#[cfg(target_os = "macos")]
fn emit_function_header(name: &str) -> String {
    let mut ctx = String::new();

    ctx.push_str(&format!("  .section __TEXT,__text\n"));
    ctx.push_str(&format!("  .globl {}\n", &name));
    ctx.push_str(&emit_label(&name));
    return ctx
}

fn compile_program(value: i64) -> String {
    let mut ctx = String::new();

    ctx.push_str(&emit_function_header("_init")[..]);
    ctx.push_str(&format!("  movq ${}, %rax\n", value));
    ctx.push_str(&format!("  retq\n"));
    return ctx;
}

// ---
// Notes:
//
// 1. Why is there no formatln! macro?
// 2. A nicer API to cat these tiny bits of strings would be great!
