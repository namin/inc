extern crate cc;

fn main() {
    cc::Build::new()
        .file("../stst.s")
        .compile("scheme-entry-lib");
}