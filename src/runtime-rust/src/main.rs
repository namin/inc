extern "C" {
    fn scheme_entry() -> u32;
}

fn main() {
   let r = unsafe { scheme_entry() };
   println!("{}", r);
}