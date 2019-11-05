
mod load;

use structopt::StructOpt;
use std::path::PathBuf;
use std::fs;

#[derive(StructOpt)]
struct Options {
    input: PathBuf,
}


fn main() {
    let options = Options::from_args();

    let bytes = fs::read(&options.input).unwrap();
    let module = load::parse_module(&bytes).unwrap();
}


