use std::fs;

use frameql_ast;
use frameql_compiler::{self, ProgNode};
use frameql_parser;

pub fn compile_from_str(prog_str: &str) -> Vec<ProgNode> {
    let ast: frameql_ast::Datalog = frameql_parser::parse(prog_str);
    let prog_nodes: Vec<ProgNode> = frameql_compiler::compile_prog(ast.into());
    println!("{:?}", prog_nodes);
    prog_nodes
}

pub fn compile_from_file(filename: &str) -> Vec<ProgNode> {
    let contents = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Error reading {}: {}", filename, e);
        std::process::exit(1);
    });
    compile_from_str(&contents)
}
