//! Command line interface for inc

use crate::{compiler::emit, core::Config, core::AST};

use std::env;
use std::fs::File;
use std::io::{self, Read, Write};
use std::process::Command;

/// Parse the command line arguments to a Config object
///
/// CLI expects the program in stdin, writes generated assembly to file name
/// specified as first argument or defaults to stdout.
pub fn parse() -> Config {
    let args: Vec<String> = env::args().collect();

    let mut program = String::new();

    io::stdin()
        .read_to_string(&mut program)
        .expect("Expected a program in stdin");

    // impl Writer
    let output = if args.len() == 1 {
        String::from("/dev/stdout")
    } else {
        args[1].clone()
    };

    Config { program, output }
}

/// Compile the program and write the assembly to target
pub fn compile(config: &Config) -> Result<(), std::io::Error> {
    let prog: AST =
        config.program.parse::<AST>().expect("Failed to parse input program");

    let mut handler = File::create(&config.asm())
        .expect(&format!("Failed to create {}", &config.asm()));

    handler.write_all(emit::program(&prog).as_bytes())
}

/// Build the generated ASM with clang into executable binary
pub fn build(config: &Config) -> bool {
    Command::new("clang")
        .arg("-m64")
        .arg("-g3")
        .arg("-ggdb3")
        .arg("-fomit-frame-pointer")
        .arg("-fno-asynchronous-unwind-tables")
        .arg("-O0")
        .arg("runtime.c")
        .arg(&config.asm())
        .arg("-o")
        .arg(&config.output)
        .status()
        .expect("Failed to compile binary")
        .success()
}

/// Run the generated binary and return output
pub fn run(config: &Config) -> Result<String, std::io::Error> {
    let proc = Command::new(&config.output)
        .output()
        .expect(&format!("Failed to run binary `{}`", &config.output));

    Ok(String::from_utf8(proc.stdout).unwrap().trim().to_string())
}
