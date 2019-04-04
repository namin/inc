// Integration tests
//
// TODO:
//
// 1. Generate asm into a unique file for each run to avoid synchronization bugs
// and to get a clean state per each run.
//

extern crate inc;

use inc::*;
use std::fs::{self};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

// Get a test config with program as input
fn config(program: String) -> Config {
    let epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    // On Mac, it's very important to run these tests in memory with tmpfs, file
    // system ordering events made the test consistently.
    let output = format!("/tmp/inc-{:?}", epoch);

    Config {
        program,
        output,
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
        .arg(&config.asm())
        .arg("-o")
        .arg(&config.output)
        .status()
        .expect("Failed to compile binary")
        .success()
}

// Run a single test, assert everything and cleanup afterwards
fn test1(input: String, output: String) {
    // Create a fresh config per run, this should allow for parallelism later.
    let mut config = config(input);

    // Rebuild before every run
    assert!(build(&mut config));

    // Run the generated binary and assert output
    let proc = Command::new(&config.output)
        .output()
        .expect(&format!("Failed to run binary `{}`", &config.output));

    assert!(&proc.status.success());
    assert_eq!(String::from_utf8(proc.stdout).unwrap().trim().replace("\n", ""), output);

    // Clean up all the intermediary files generated
    fs::remove_file(&config.asm()).expect("Failed to clear generated asm files");
    fs::remove_file(&config.output).expect("Failed to clear executable");
    fs::remove_dir_all(format!("{}.dSYM", &config.output)).expect("Failed to rm a.out.dSYM");
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
        test1(String::from(*inp), String::from(*out));
    }
}

#[quickcheck]
fn qc_int64(i: i64) -> () {
    test1(i.to_string(), i.to_string())
}
