mod ast;
mod error;
mod executor;
mod load;
mod module;

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

    let module = module::Module::from_ast(module)?;

    dbg!(&module);

    let file_name = options
        .input
        .file_stem()
        .ok_or_else(|| err!("path did not contain a file name: {}", options.input.display()))?
        .to_string_lossy();

    let mut executor = executor::Executor::new();
    executor.instantiate_env()?;
    let _instance = executor.instantiate(file_name, module)?;

    dbg!(executor);
    dbg!(_instance);

    Ok(())
}
