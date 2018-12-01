use std::io::{self, Read};
use std::collections::HashSet;


fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	println!("{}", deliver(&input));
}

#[derive(Hash, Eq, PartialEq, Copy, Clone)]
struct Coordinate {
	x: i32,
	y: i32,
}

fn deliver(input: &str) -> usize {
	let mut santas = [
		Coordinate { x: 0, y: 0 },
		Coordinate { x: 0, y: 0 },
	];
	let mut visited: HashSet<Coordinate> = HashSet::new();
	visited.insert(santas[0]);

	for (i, c) in input.chars().enumerate() {
		let santa = &mut santas[i % 2];
		match c {
			'>' => santa.x += 1,
			'<' => santa.x -= 1,
			'^' => santa.y += 1,
			'v' => santa.y -= 1,
			_ => panic!("Don't know how to handle {}", c)
		}

		visited.insert(*santa);
	}

	visited.len()
}

#[test]
fn test_deliver() {
	assert_eq!(deliver("^v"), 3);
	assert_eq!(deliver("^>v<"), 3);
	assert_eq!(deliver("^v^v^v^v^v"), 11);
}
