use std::{error::Error};

use rclox::lox::Lox;

fn main() -> Result<(), Box<dyn Error>> {
    let lox = Lox::new();
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 0 {
        match lox.repl() {
            Ok(result) => println!("Result: {:?}", result),
            Err(failure) => println!("Failure: {}", failure.to_string())
        };
    } else if args.len() == 1 {
        match lox.run_file(&args[1]) {
            Ok(result) => println!("Result: {:?}", result),
            Err(failure) => println!("Failure: {}", failure.to_string())
        };
    } else {
        println!("Usage: clox [path]");
        std::process::exit(64);
    }

    Ok(())
}
