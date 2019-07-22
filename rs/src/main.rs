extern crate getopts;
extern crate inc;

use getopts::Options;
use inc::{cli, core::Config};
use std::env;
use std::io::{self, Read};

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let bin = args[0].clone();

    let mut opts = Options::new();
    opts.optopt("o", "", "Output file name", "FILE");
    opts.optflag("S", "", "Print generated asm");
    opts.optflag("h", "help", "print this help menu");

    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    if matches.opt_present("h") {
        let brief = format!("Usage: {} [options]", bin);
        print!("{}", opts.usage(&brief));
        return Ok(());
    }

    let exec = !matches.opt_present("S");
    let output = if exec {
        matches.opt_str("o").unwrap_or_else(|| String::from("inc"))
    } else {
        matches.opt_str("o").unwrap_or_else(|| String::from("/dev/stdout"))
    };

    let mut program = String::new();
    io::stdin()
        .read_to_string(&mut program)
        .expect("Expected a program in stdin");

    let config = Config { program, output, exec };

    cli::compile(&config)?;

    if config.exec {
        cli::build(&config);
        let out = cli::run(&config)?;
        println!("{}", out);
    }

    Ok(())
}
