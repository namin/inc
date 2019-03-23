// Integration tests

// use super::*;
use std::io::Write;
use std::process::{Command, Output, Stdio};

fn build(input: &[u8]) -> bool {
    let mut exe = Command::new("make")
        .arg("--quiet")
        .arg("a.out")
        .stdin(Stdio::piped())
        .spawn()
        .expect("Failed to compile binary");

    exe.stdin
        .as_mut()
        .expect("Failed to get stdin")
        .write(input)
        .expect("Failed to write to stdin");

    return exe.wait().expect("Failed to wait for completion").success();
}

fn exec() -> Output {
    return Command::new("make").output().expect("Failed to run binary");
}

fn run(input: &[u8]) -> std::vec::Vec<u8> {
    assert!(build(input));
    let proc = exec();
    assert!(proc.status.success());
    return proc.stdout;
}

#[test]
fn it_builds() {
    assert!(build(b"42"));
}

#[test]
fn it_runs() {
    assert_eq!(run(b"42"), Vec::from("42\n"));
}

#[test]
fn it_integers() {
    let tests = vec![
        ("0", "0\n"),
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
        assert_eq!(run(inp.as_bytes()), Vec::from(*out));
    }
}
