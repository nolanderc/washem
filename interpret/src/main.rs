mod ast;
mod error;
mod executor;
mod load;
mod module;

use crate::error::*;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;
use std::time::Instant;

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

    let module = time("parse", || load::parse_module(&bytes))?;

    let module = time("ast", || module::Module::from_ast(module))?;

    let file_name = options
        .input
        .file_stem()
        .ok_or_else(|| err!("path did not contain a file name: {}", options.input.display()))?
        .to_string_lossy();

    let mut executor = executor::Executor::new();
    executor.instantiate_env()?;
    let instance = time("instantiate", || executor.instantiate(file_name, module))?;

    let main_function = instance.function("main")?;

    let result = time("execute", || executor.call(main_function, vec![]))?;

    dbg!(result);

    Ok(())
}

fn time<T>(name: &str, f: impl FnOnce() -> T) -> T {
    let start = Instant::now();
    let result = f();
    eprintln!("{}: {}ms", name, start.elapsed().as_secs_f64() * 1000.0);

    result
}

