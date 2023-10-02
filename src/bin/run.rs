use std::{env::args, fs::read_to_string};

use tapl::run;

fn main() {
    let path = args().nth(1).unwrap_or("input.f".to_string());
    let src = read_to_string(path).expect("file not found");
    run(&src);
}
