use std::io;
use std::io::prelude::*;
use std::collections::HashSet;
use std::iter::Iterator;

#[derive(Eq)]
#[derive(PartialEq)]
#[derive(Hash)]
#[derive(Clone)]
#[derive(Debug)]
struct Coord {
	x: isize,
	y: isize,
}

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input).unwrap();
	let scaffolding = parse_input(input);

	let result = scaffolding.iter()
		.filter(|c| has_four_neighbors((**c).clone(), &scaffolding))
		.map(|c| c.x * c.y)
		.sum::<isize>();
	println!("{}", result);
}

fn parse_input(input: String) -> HashSet<Coord> {
	let mut result = HashSet::new();

	for (y, line) in input.split("\n").enumerate() {
		for (x, c) in line.chars().enumerate() {
			if c == '#' {
				result.insert(Coord{ x: x as isize, y: y as isize});
			}
		}
	}

	result
}

fn has_four_neighbors(c: Coord, scaffolding: &HashSet<Coord>) -> bool {
	scaffolding.contains(&Coord{ x: c.x, y: c.y - 1 })
		&& scaffolding.contains(&Coord{ x: c.x, y: c.y + 1 })
		&& scaffolding.contains(&Coord{ x: c.x - 1, y: c.y })
		&& scaffolding.contains(&Coord{ x: c.x + 1, y: c.y })
}
