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

    Ok(compile_program(i))
}

fn emit_label(label: String) {
    println!("{}:", label);
}

#[cfg(target_os = "macos")]
fn emit_function_header(name: String) {
    println!("  .section __TEXT,__text");
    println!("  .globl {}", name);
    emit_label(name)
}

fn compile_program(value: i64) {
    emit_function_header(String::from("_init"));
    println!("  movq ${}, %rax", value);
    println!("  retq");
}
