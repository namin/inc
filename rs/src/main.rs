use std::env;
use std::fs::File;
use std::io::{self, Read, Write};

// Usage: Expects the program in stdin, writes generated assembly to file name
// specified as first argument or defaults to stdout.
//
fn main() -> std::io::Result<()> {
    let mut config = Config::new();

    let i: i64 = config
        .program
        .trim_end()
        .parse()
        .expect("Failed to parse stdin to a valid program");

    config
        .outfile
        .write_all(compile_program(i).as_bytes())
        .expect("Failed to write generated code");

    Ok(())
}

struct Config {
    program: String,
    outfile: File,
}

impl Config {
    fn new() -> Config {
        let args: Vec<String> = env::args().collect();

        let mut program = String::new();

        io::stdin()
            .read_to_string(&mut program)
            .expect("Expected a program in stdin");

        // impl Writer
        let outfile = if args.len() == 1 {
            File::open("/dev/stdout").unwrap()
        } else {
            File::create(args[1].clone()).expect("Failed to open output file")
        };

        Config { program, outfile }
    }
}

fn emit_label(label: &str) -> String {
    format!("{}:\n", label)
}

#[cfg(target_os = "macos")]
fn emit_function_header(name: &str) -> String {
    let mut ctx = String::new();

    ctx.push_str("  .section __TEXT,__text\n");
    ctx.push_str(&format!("  .globl {}\n", &name));
    ctx.push_str(&emit_label(&name));
    ctx
}

fn compile_program(value: i64) -> String {
    let mut ctx = String::new();

    ctx.push_str(&emit_function_header("_init")[..]);
    ctx.push_str(&format!("  movq ${}, %rax\n", value));
    ctx.push_str("  retq\n");
    ctx
}

// ---
// Notes:
//
// 1. Why is there no formatln! macro?
// 2. A nicer API to cat these tiny bits of strings would be great!
