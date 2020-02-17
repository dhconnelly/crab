mod ast;
mod code;
mod compiler;
mod parser;
mod scanner;
mod token;
mod vm;

use std::env;
use std::error;
use std::fs;

fn main() -> Result<(), Box<dyn error::Error>> {
    let args: Vec<_> = env::args().skip(1).collect();
    if args.len() == 0 {
        eprintln!("usage: crab filename...");
        std::process::exit(1);
    }
    for path in args {
        println!("executing: {}", path);
        let text = fs::read_to_string(&path)?;
        let toks = scanner::scan(&text);
        let tree = parser::parse(toks)?;
        let code = compiler::compile(&tree)?;
        vm::execute(&code)?;
    }
    Ok(())
}
