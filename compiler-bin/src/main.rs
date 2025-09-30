use std::env;

use frameql::compile_from_file;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    compile_from_file(&args[1]);

    // Print or use the result
    // println!("{:#?}", prog_nodes);
}
