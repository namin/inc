extern crate cc;
extern crate bindgen;

fn main() {
    cc::Build::new()
        .file("../stst.s")
        .compile("scheme-entry-lib");

    let bindings = bindgen::Builder::default()
        .header("bindings.h")
        .generate()
        .expect("Unable to generate bindings");
    bindings
        .write_to_file("src/bindings.rs")
        .expect("Couldn't write bindings!");
}
