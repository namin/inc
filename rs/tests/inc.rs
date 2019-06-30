// Integration tests
extern crate inc;

use inc::core::*;
use std::fs;
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
        fn simple() {
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

// Step 6. Conditionals
mod cond {
    use super::*;

    #[test]
    fn simple() {
        let tests = [
            ("(if #t 12 13)", "12"),
            ("(if #f 12 13)", "13"),
            ("(if 0 12 13)", "12"),
            ("(if () 43 ())", "43"),
            ("(if #t (if 12 13 4) 17)", "13"),
            ("(if #f 12 (if #f 13 4))", "4"),
            ("(if #\\X (if 1 2 3) (if 4 5 6))", "2"),
            ("(if (not (boolean? #t)) 15 (boolean? #f))", "#t"),
            (
                "(if (if (char? #\\a) (boolean? #\\b) (fixnum? #\\c)) 119 -23)",
                "-23",
            ),
            ("(if (if (if (not 1) (not 2) (not 3)) 4 5) 6 7)", "6"),
            ("(if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7)", "7"),
            (
                "(not (if (not (if (if (not 1) (not 2) (not 3)) 4 5)) 6 7))",
                "#f",
            ),
            ("(if (char? 12) 13 14)", "14"),
            ("(if (char? #\\a) 13 14)", "13"),
            ("(inc (if (dec 1) (dec 13) 14))", "13"),
            // conditionals
            ("(if (= 12 13) 12 13)", "13"),
            ("(if (= 12 12) 13 14)", "13"),
            ("(if (< 12 13) 12 13)", "12"),
            ("(if (< 12 12) 13 14)", "14"),
            ("(if (< 13 12) 13 14)", "14"),
            ("(if (<= 12 13) 12 13)", "12"),
            ("(if (<= 12 12) 12 13)", "12"),
            ("(if (<= 13 12) 13 14)", "14"),
            ("(if (> 12 13) 12 13)", "13"),
            ("(if (> 12 12) 12 13)", "13"),
            ("(if (> 13 12) 13 14)", "13"),
            ("(if (>= 12 13) 12 13)", "13"),
            ("(if (>= 12 12) 12 13)", "12"),
            ("(if (>= 13 12) 13 14)", "13"),
        ];
        test_many(&tests)
    }
}

// Step 7: Pairs in heap
mod heap {
    use super::*;

    #[test]
    fn simple() {
        let tests = [
            ("(pair? (cons 1 2))", "#t"),
            ("(pair? (cons 1 2))", "#t"),
            ("(pair? 12)", "#f"),
            ("(pair? #t)", "#f"),
            ("(pair? #f)", "#f"),
            ("(pair? ())", "#f"),
            ("(fixnum? (cons 12 43))", "#f"),
            ("(boolean? (cons 12 43))", "#f"),
            ("(null? (cons 12 43))", "#f"),
            ("(not (cons 12 43))", "#f"),
            ("(if (cons 12 43) 32 43)", "32"),
            ("(car (cons 1 23))", "1"),
            ("(cdr (cons 43 123))", "123"),
            ("(car (car (cons (cons 12 3) (cons #t #f))))", "12"),
            ("(cdr (car (cons (cons 12 3) (cons #t #f))))", "3"),
            ("(car (cdr (cons (cons 12 3) (cons #t #f))))", "#t"),
            ("(cdr (cdr (cons (cons 12 3) (cons #t #f))))", "#f"),
            ("(let
                 ((x (let
                        ((y (+ 1 2)))
                       (* y y))))
                (cons x (+ x x)))", "(9 . 18)"),

            ("(let ((t (cons 1 2))) (let ((t t)) (let ((t t)) (let ((t t)) t))))",  "(1 . 2)"),

            ("(let ((t (let ((t (let ((t (let ((t (cons 1 2))) t))) t))) t))) t)", "(1 . 2)"),

            ("(let ((x ()))
                (let ((x (cons x x)))
                  (let ((x (cons x x)))
                    (let ((x (cons x x)))
                      (cons x x)))))", "((((()) ()) (()) ()) ((()) ()) (()) ())")
        ];

        test_many(&tests)
    }
}

// Get a test config with program as input
fn config(program: String) -> Config {
    // Time epoch instead of UUID occasionally ran into race conditions which
    // made multiple tests write to the same file concurrently completely
    // messing things up.
    let output = format!("inc-{:?}", Uuid::new_v4());

    Config { program, output, exec: true }
}

// Build an executable with generated asm
fn build(config: &Config) -> bool {
    inc::cli::compile(&config).unwrap();
    inc::cli::build(&config)
}

fn test_many(tests: &[(&str, &str)]) {
    for (inp, out) in tests.iter() {
        test1(inp, out);
    }
}

// Run a single test, assert everything and cleanup afterwards
fn test1(input: &str, output: &str) {
    // Create a fresh config per run, this should allow for parallelism later.
    let mut config = config(input.to_string());

    // Rebuild before every run
    assert!(build(&mut config));

    // Run the generated binary and assert output
    assert_eq!(
        inc::cli::run(&config).unwrap(),
        output,
        "Failed: {} != {}",
        input,
        output
    );

    // Clean up all the intermediary files generated
    fs::remove_file(&config.asm())
        .expect("Failed to clear generated asm files");
    fs::remove_file(&config.output).expect("Failed to clear executable");
    fs::remove_dir_all(format!("{}.dSYM", &config.output)).unwrap_or_default()
}
