use std::io::{self, Read};
use std::collections::HashSet;


fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	println!("{}", deliver(&input));
}

fn deliver(input: &str) -> usize {
	//let mut visited : Vec<[i32; 2]> = Vec::new();
	let mut x = 0;
	let mut y = 0;
	let mut visited: HashSet<[i32; 2]> = HashSet::new();
	visited.insert([x, y]);

	for c in input.chars() {
		match c {
			'>' => x += 1,
			'<' => x -= 1,
			'^' => y += 1,
			'v' => y -= 1,
			_ => panic!("Don't know how to handle {}", c)
		}

		visited.insert([x, y]);
	}

	visited.len()
}

#[test]
fn test_deliver() {
	assert_eq!(deliver(">"), 2);
	assert_eq!(deliver("^>v<"), 4);
	assert_eq!(deliver("^v^v^v^v^v"), 2);
}
