//! Command line interface for inc

use crate::{compiler::emit, core::Config, core::AST};

use std::fs::File;
use std::io::Write;
use std::process::Command;

/// Compile the program and write the assembly to target
pub fn compile(config: &Config) -> Result<(), std::io::Error> {
    let prog: AST = config
        .program
        .parse::<AST>()
        .expect(&format!("Failed to parse input program `{}`", config.program));

    let mut handler = File::create(&config.asm())
        .unwrap_or_else(|_| panic!("Failed to create {}", &config.asm()));

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
    let proc = Command::new(format!("./{}", &config.output))
        .output()
        .unwrap_or_else(|e| {
            panic!("Failed to run binary `{}`; error: `{}`", &config.output, e)
        });

    Ok(String::from_utf8(proc.stdout).unwrap().trim().to_string())
}
