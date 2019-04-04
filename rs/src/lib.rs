// ---
// TODO:
//
// 1. Why is there no formatln! macro?
// 2. A nicer API to cat these tiny bits of strings would be great!
// ---

use std::fs::File;
use std::io::Write;

// Control behavior and external interaction of the program.
pub struct Config {
    // Program is the input source
    pub program: String,
    // Outfile is the name of the generated asm and executable
    pub output: String,
}

impl Config  {
    pub fn asm(&self) -> String {
        format!("{}.s", self.output)
    }
}

// Custom error type for all of inc. This might not be idiomatic Rust, revisit
// later.
#[derive(Debug)]
pub struct Error {
    message: String,
}

// Parse the input from user into the form the top level of the compiler
// understands.
//
// TODO: There must be much nice idiomatic ways to do this without unwrapping
// and wrapping the errors again.
//
fn parse(program: &str) -> Result<i64, Error> {
    match program.trim_end().parse::<i64>() {
        Ok(i) => Ok(i),
        Err(_) => Err(Error {
            message: String::from("Failed to parse stdin to a valid program"),
        }),
    }
}

pub fn compile(config: &mut Config) -> Result<(), Error> {
    let i: i64 = parse(&config.program)?;

    let asm = format!("{}.s", &config.output);
    let mut handler = File::create(&asm)
        .expect(&format!("Failed to create {}", &asm));

    match handler.write_all(emit::program(i).as_bytes()) {
        Ok(_) => Ok(()),
        Err(e) => Err(Error {
            message: format!("Failed to write generated code: {}", e),
        }),
    }
}

#[cfg(target_os = "macos")]
mod emit {
    fn label(label: &str) -> String {
        format!("{}:\n", label)
    }

    fn function_header(name: &str) -> String {
        let mut ctx = String::new();

        ctx.push_str("  .section __TEXT,__text\n");
        ctx.push_str(&format!("  .globl {}\n", &name));
        ctx.push_str(&label(&name));
        ctx
    }

    pub fn program(value: i64) -> String {
        let mut ctx = String::new();

        ctx.push_str(&function_header("_init")[..]);
        ctx.push_str(&format!("  movq ${}, %rax\n", value));
        ctx.push_str("  retq\n");
        ctx
    }
}
