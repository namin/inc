// Integration tests
extern crate inc;

use inc::*;
use std::{fs, process::Command};
use uuid::Uuid;

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

// Step 1: Integers
mod integers {
    use super::*;

    #[test]
    fn unit() {
        for t in &[0, 1, -1, 10, -10, 2736, -2736, 536870911, -536870912] {
            test1(&t.to_string(), &t.to_string());
        }
    }

    #[quickcheck]
    fn quick(i: i64) -> () {
        test1(&i.to_string(), &i.to_string())
    }
}

// Step 2: Immediate constants
mod immediate {
    use super::*;

    #[test]
    fn unit() {
        let tests = [
            r"#f",
            r"#t",
            "()",
            r"#\tab",
            r"#\newline",
            r"#\return",
            r"#\space",
            r"#\!",
            r"#\#",
            r"#\$",
            r"#\%",
            r"#\&",
            r"#\'",
            r"#\(",
            r"#\)",
            r"#\*",
            r"#\+",
            r"#\,",
            r"#\-",
            r"#\.",
            r"#\/",
            r"#\0",
            r"#\9",
            r"#\:",
            r"#\;",
            r"#\<",
            r"#\=",
            r"#\>",
            r"#\?",
            r"#\@",
            r"#\A",
            r"#\B",
            r"#\Z",
            r"#\(",
            r"#\\",
            r"#\]",
            r"#\^",
            r"#\_",
            r"#\`",
            r"#\a",
            r"#\b",
            r"#\z",
            r"#\{",
            r"#\|",
            r"#\}",
            r"#\~",
        ];

        for t in tests.iter() {
            test1(t, t);
        }
    }
}

// Step 3: Unary Primitives
mod unary {
    use super::*;

    #[test]
    fn inc() {
        let tests = vec![
            ("(inc 0)", "1"),
            ("(inc -1)", "0"),
            ("(inc 41)", "42"),
            ("(inc -100) ", "-99"),
            ("(inc (inc (inc (inc 1))))", "5"),
        ];

        for (inp, out) in tests.iter() {
            test1(inp, out);
        }
    }

    #[test]
    fn types() {
        let tests = [
            ("(fixnum? 0)", "#t"),
            ("(fixnum? 1)", "#t"),
            ("(fixnum? -1)", "#t"),
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
            test1(inp, out);
        }
    }

    #[test]
    fn zero() {
        let tests = vec![
            (r"(zero? 0)", r"#t"),
            (r"(zero? 1)", r"#f"),
            (r"(zero? #t)", r"#f"),
            (r"(zero? ())", r"#f"),
        ];

        for (inp, out) in tests.iter() {
            test1(inp, out);
        }
    }

    #[test]
    fn null() {
        let tests = vec![
            (r"(null? ())", r"#t"),
            (r"(null? #\Q)", r"#f"),
            (r"(null? #f)", r"#f"),
            (r"(null? #t)", r"#f"),
            (r"(null? -23873)", r"#f"),
            (r"(null? 37287)", r"#f"),
            (r"(null? #\a)", r"#f"),
        ];

        for (inp, out) in tests.iter() {
            test1(inp, out);
        }
    }

}

// Step 4: Binary primitives
mod binary {
    mod unit {
        use super::super::*;

        #[test]
        fn plus() {
            let tests = [("(+ 10 20)", "30"), ("(+ 40 (inc (inc 0)))", "42")];

            for (inp, out) in tests.iter() {
                test1(inp, out);
            }
        }
    }

    mod quick {
        use super::super::*;

        #[quickcheck]
        fn plus(x: i64, y: i64) -> () {
            test1(&format!("(+ {} {})", x, y), &(x + y).to_string())
        }

        #[quickcheck]
        fn multiply(x: i64, y: i64) -> () {
            test1(&format!("(* {} {})", x, y), &(x * y).to_string())
        }

        #[quickcheck]
        fn divide(x: i64, y: i64) -> () {
            if y != 0 {
                test1(&format!("(/ {} {})", (x * y), y), &x.to_string())
            }
        }

        #[quickcheck]
        fn remainder(x: i64, y: i64) -> () {
            if y != 0 {
                test1(&format!("(% {} {})", x, y), &((x % y).to_string()))
            }
        }
    }
}

// Step 5: Let bindings
mod bindings {
    mod unit {
        use super::super::*;

        #[test]
        fn most_trivial_let_bindings() {
            let tests = [
                ("(let ((x 5)) x)", "5"),
                ("(let ((x 5) (y 4)) (+ x y))", "9"),
                ("(let ((x (+ 1 2))) x)", "3"),
            ];

            for (inp, out) in tests.iter() {
                test1(inp, out);
            }
        }

        #[test]
        fn nested_scopes() {
            let tests =
                [("(let ((x (+ 1 2))) (let ((y (+ 3 4))) (+ x y)))", "10")];

            for (inp, out) in tests.iter() {
                test1(inp, out);
            }
        }

        #[test]
        fn shadow() {
            let tests = [("(let ((x 1)) (let ((x 2)) #t) x)", "1")];

            for (inp, out) in tests.iter() {
                test1(inp, out);
            }
        }
    }

}

// Get a test config with program as input
fn config(program: String) -> Config {
    // Time epoch instead of UUID occasionally ran into race conditions which
    // made multiple tests write to the same file concurrently completely
    // messing things up.
    let output = format!("/tmp/inc-{:?}", Uuid::new_v4());

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
fn test1(input: &str, output: &str) {
    // Create a fresh config per run, this should allow for parallelism later.
    let mut config = config(input.to_string());

    // Rebuild before every run
    assert!(build(&mut config));

    // Run the generated binary and assert output
    let proc = Command::new(&config.output)
        .output()
        .expect(&format!("Failed to run binary `{}`", &config.output));

    assert!(
        &proc.status.success(),
        format!("Failed to run binary `{}`", &config.output)
    );

    assert_eq!(String::from_utf8(proc.stdout).unwrap().trim(), output);

    // Clean up all the intermediary files generated
    fs::remove_file(&config.asm())
        .expect("Failed to clear generated asm files");
    fs::remove_file(&config.output).expect("Failed to clear executable");
    fs::remove_dir_all(format!("{}.dSYM", &config.output)).unwrap_or_default()
}
