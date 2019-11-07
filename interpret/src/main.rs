mod ast;
mod error;
mod executor;
mod load;

use crate::error::*;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Options {
    input: PathBuf,
}

fn main() {
    let options = Options::from_args();

    match start(&options) {
        Ok(()) => eprintln!("done"),
        Err(e) => eprintln!("Error: {}", e),
    }
}

fn start(options: &Options) -> Result<()> {
    let bytes = fs::read(&options.input)?;
    let module = load::parse_module(&bytes)?;

    dbg!(module);

    Ok(())
}
