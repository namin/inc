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

// Test driver

#[cfg(test)]
mod tests {
    // use super::*;
    use std::io::Write;
    use std::process::{Command, Output, Stdio};

    fn build(input: &[u8]) -> bool {
        let mut exe = Command::new("make")
            .arg("--quiet")
            .arg("a.out")
            .stdin(Stdio::piped())
            .spawn()
            .expect("Failed to compile binary");

        exe.stdin
            .as_mut()
            .expect("Failed to get stdin")
            .write(input)
            .expect("Failed to write to stdin");

        return exe.wait().expect("Failed to wait for completion").success();
    }

    fn exec() -> Output {
        return Command::new("make").output().expect("Failed to run binary");
    }

    fn run(input: &[u8]) -> std::vec::Vec<u8> {
        assert!(build(input));
        let proc = exec();
        assert!(proc.status.success());
        return proc.stdout;
    }

    #[test]
    fn it_builds() {
        assert!(build(b"42"));
    }

    #[test]
    fn it_runs() {
        assert_eq!(run(b"42"), Vec::from("42\n"));
        assert_eq!(run(b"36"), Vec::from("36\n"));
    }

}
