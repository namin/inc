extern crate inc;

use inc::Config;
use std::env;
use std::io::{self, Read};

fn main() -> std::io::Result<()> {
    let mut config = cli();

    inc::compile(&mut config).unwrap();

    Ok(())
}

// Config controls the external interaction of the program by providing it with
// input and giving the compiler a file to write results to.
//
// Cli expects the program in stdin, writes generated assembly to file name
// specified as first argument or defaults to stdout.
//
fn cli() -> Config {
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


    Config {
        program,
        output,
    }
}
