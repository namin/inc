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

// Test driver

#[cfg(test)]
mod tests {
    // use super::*;
    use std::process::Command;

    fn build() -> bool {
        return Command::new("make")
            .arg("--quiet")
            .arg("a.out")
            .status()
            .expect("Failed to compile binary")
            .success();
    }

    #[test]
    fn it_builds() {
        assert!(build());
    }
}
