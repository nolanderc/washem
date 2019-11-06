
mod error;
mod load;

use structopt::StructOpt;
use std::path::PathBuf;
use std::fs;
use crate::error::*;

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

