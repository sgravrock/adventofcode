use std::io::{self, Read, Write};

fn main() {
	print!("Input: ");
	io::stdout().flush()
		.expect("Failed to write to stdout");
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");

	let mut floor = 0;

	for c in input.chars() {
		if c == '(' {
			floor +=  1;
		} else if c == ')' {
			floor -=  1;
		}
	}

	println!("Floor {}", floor);
}
