extern crate inc;

use inc::cli;

fn main() -> Result<(), std::io::Error> {
    let config = cli::parse();
    cli::compile(&config)
}
