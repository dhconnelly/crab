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
    let path = env::args().skip(1).next().ok_or("Usage: crab <filename>")?;
    let text = fs::read_to_string(&path)?;
    let toks = scanner::scan(&text);
    let tree = parser::parse(toks)?;
    println!("{:#?}", tree);
    let code = compiler::compile(&tree)?;
    println!("{:#?}", code);
    vm::execute(&code)?;
    Ok(())
}
