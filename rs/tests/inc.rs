// Integration tests
extern crate inc;

use inc::*;
use std::fs;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

mod steps {
    use super::*;

    // Step 1: Integers
    #[test]
    fn integers() {
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

    // Step 2: Immediate constants
    #[test]
    fn constants() {
        let tests = vec![
            (r"#f", r"#f"),
            (r"#t", r"#t"),
            ("()", r"()"),
            (r"#\tab", r"#\tab"),
            (r"#\newline", r"#\newline"),
            (r"#\return", r"#\return"),
            (r"#\space", r"#\space"),
            (r"#\!", r"#\!"),
            (r"#\#", r"#\#"),
            (r"#\$", r"#\$"),
            (r"#\%", r"#\%"),
            (r"#\&", r"#\&"),
            (r"#\'", r"#\'"),
            (r"#\(", r"#\("),
            (r"#\)", r"#\)"),
            (r"#\*", r"#\*"),
            (r"#\+", r"#\+"),
            (r"#\,", r"#\,"),
            (r"#\-", r"#\-"),
            (r"#\.", r"#\."),
            (r"#\/", r"#\/"),
            (r"#\0", r"#\0"),
            (r"#\9", r"#\9"),
            (r"#\:", r"#\:"),
            (r"#\;", r"#\;"),
            (r"#\<", r"#\<"),
            (r"#\=", r"#\="),
            (r"#\>", r"#\>"),
            (r"#\?", r"#\?"),
            (r"#\@", r"#\@"),
            (r"#\A", r"#\A"),
            (r"#\B", r"#\B"),
            (r"#\Z", r"#\Z"),
            (r"#\(", r"#\("),
            (r"#\\", r"#\\"),
            (r"#\]", r"#\]"),
            (r"#\^", r"#\^"),
            (r"#\_", r"#\_"),
            (r"#\`", r"#\`"),
            (r"#\a", r"#\a"),
            (r"#\b", r"#\b"),
            (r"#\z", r"#\z"),
            (r"#\{", r"#\{"),
            (r"#\|", r"#\|"),
            (r"#\}", r"#\}"),
            (r"#\~", r"#\~"),
        ];

        for (inp, out) in tests.iter() {
            test1(String::from(*inp), String::from(*out));
        }
    }

    // Step 3: Unary functions
    mod unary {
        use super::*;

        #[test]
        fn inc() {
            let tests = vec![
                (r"(inc 0)", r"1"),
                (r"(inc -1)", r"0"),
                (r"(inc 41)", r"42"),
                (r"(inc -100) ", r"-99"),
                (r"(inc 1000) ", r"1001"),
                (r"(inc 536870910) ", r"536870911"),
                (r"(inc -536870912) ", r"-536870911"),
            ];

            for (inp, out) in tests.iter() {
                test1(String::from(*inp), String::from(*out));
            }
        }

        #[test]
        fn types() {
            let tests = vec![
                ("(fixnum? 0)", "#t"),
                ("(fixnum? 1)", "#t"),
                ("(fixnum? -1)", "#t"),
                ("(fixnum? 37287)", "#t"),
                ("(fixnum? -23873)", "#t"),
                ("(fixnum? 536870911)", "#t"),
                ("(fixnum? -536870912)", "#t"),
                ("(fixnum? #t)", "#f"),
                ("(fixnum? #f)", "#f"),
                ("(fixnum? ())", "#f"),
                ("(fixnum? #\\Q)", "#f"),
                ("(char? #\\Q)", "#t"),
                ("(char? 0)", "#f"),
                ("(char? 1)", "#f"),
                ("(char? -1)", "#f"),
                ("(char? 37287)", "#f"),
                ("(char? -23873)", "#f"),
                ("(char? 536870911)", "#f"),
                ("(char? -536870912)", "#f"),
                ("(char? #t)", "#f"),
                ("(char? #f)", "#f"),
                ("(char? ())", "#f"),
                ("(not 1)", "#f"),
                ("(not #t)", "#f"),
                ("(not #f)", "#t"),
                ("(not ())", "#f"),
            ];

            for (inp, out) in tests.iter() {
                test1(String::from(*inp), String::from(*out));
            }
        }

        #[test]
        fn zn() {
            let tests = vec![
                (r"(zero? 0)", r"#t"),
                (r"(zero? 1)", r"#f"),
                (r"(zero? #t)", r"#f"),
                (r"(zero? ())", r"#f"),
                (r"(null? ())", r"#t"),
                (r"(null? #\Q)", r"#f"),
                (r"(null? #f)", r"#f"),
                (r"(null? #t)", r"#f"),
                (r"(null? -23873)", r"#f"),
                (r"(null? 37287)", r"#f"),
                (r"(null? #\a)", r"#f"),
            ];

            for (inp, out) in tests.iter() {
                test1(String::from(*inp), String::from(*out));
            }
        }
    }
}

mod quick {
    use super::*;

    #[quickcheck]
    fn int64(i: i64) -> () {
        test1(i.to_string(), i.to_string())
    }
}

// Get a test config with program as input
fn config(program: String) -> Config {
    let epoch = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards");

    // On Mac, it's very important to run these tests in memory with tmpfs, file
    // system ordering events made the test consistently.
    let output = format!("/tmp/inc-{:?}", epoch);

    Config { program, output }
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
    assert_eq!(
        String::from_utf8(proc.stdout)
            .unwrap()
            .trim()
            .replace("\n", ""),
        output
    );

    // Clean up all the intermediary files generated
    fs::remove_file(&config.asm()).expect("Failed to clear generated asm files");
    fs::remove_file(&config.output).expect("Failed to clear executable");
    fs::remove_dir_all(format!("{}.dSYM", &config.output)).unwrap_or_default()
}
