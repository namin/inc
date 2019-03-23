fn main() {
    compile_program(42);
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
