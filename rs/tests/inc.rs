// Integration tests
//
// TODO:
//
// 1. Generate asm into a unique file for each run to avoid synchronization bugs
// and to get a clean state per each run.
//

extern crate inc;

use inc::*;
use std::fs::{self, File};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

// Get a test config with program as input
fn config(program: String) -> Config {
    let start = SystemTime::now();
    let since_the_epoch = start
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    let outpath = format!("inc-{:?}.s", since_the_epoch);
    let outfile =
        File::create(&outpath).expect(&format!("Failed to create temp file {}", &outpath));

    Config {
        program,
        outfile,
        outpath,
    }
}

// Build an executable with generated asm
fn build(mut config: &mut Config) -> bool {
    inc::compile(&mut config).unwrap();

    Command::new("clang")
        .arg("-m64")
        .arg("-g3")
        .arg("-ggdb3")
        .arg("-fomit-frame-pointer")
        .arg("-fno-asynchronous-unwind-tables")
        .arg("-O0")
        .arg("runtime.c")
        .arg(&config.outpath)
        .status()
        .expect("Failed to compile binary")
        .success()
}

// Run a single test, assert everything and cleanup afterwards
fn test1(input: String, output: Vec<u8>) {
    // Create a fresh config per run, this should allow for parallelism later.
    let mut config = config(input);

    // Rebuild before every run
    assert!(build(&mut config));

    // Run the generated binary and assert output
    let proc = Command::new("./a.out")
        .output()
        .expect("Failed to run binary");

    assert!(&proc.status.success());
    assert_eq!(proc.stdout, output);

    // Clean up all the intermediary files generated
    fs::remove_file(&config.outpath).expect("Failed to clear generated asm files");
    fs::remove_file("a.out").expect("Failed to rm a.out");
    fs::remove_dir_all("a.out.dSYM").expect("Failed to rm a.out.dSYM");
}

#[test]
fn it_integers() {
    let tests = vec![
        ("0", "0"),
        ("1", "1"),
        ("-1", "-1"),
        ("10", "10"),
        ("-10", "-10"),
        ("2736", "2736"),
        ("-2736", "-2736"),
        ("536870911", "536870911"),
        ("-536870912", "-536870912"),
    ];

    for (inp, out) in tests.iter() {
        test1(String::from(*inp), Vec::from(format!("{}\n", &out)));
    }
}
