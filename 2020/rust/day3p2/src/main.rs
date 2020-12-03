mod input;
use std::collections::HashMap;

fn main() {
    println!("{}", n_trees_visited(&Grid::parse(input::puzzle_input())));
}

#[derive(PartialEq, Eq, Hash, Debug)]
struct Coord { x: usize, y: usize }


#[derive(PartialEq, Eq, Debug)]
struct Grid {
	pattern: HashMap<Coord, char>,
	max: Coord,
}

impl Grid {
	fn parse(input: &str) -> Grid {
		let mut pattern: HashMap<Coord, char> = HashMap::new();
		let mut max_y = 0;
		let mut max_x = 0;

		for (y, line) in input.lines().enumerate() {
			for (x, c) in line.chars().enumerate() {
				pattern.insert(Coord {x, y}, c);
				max_x = x; // assume a rectangle
				max_y = y;
			}
		}

		Grid { pattern, max: Coord { x: max_x, y: max_y } }
	}

	fn tree_at(&self, pos: Coord) -> bool {
		let adjusted = Coord {
			x: pos.x % (self.max.x + 1),
			y: pos.y
		};
		let space = self.pattern.get(&adjusted).unwrap();
		*space == '#'
	}
}

#[test]
fn test_parse() {
	let input = "..#
#..";
	let mut expected = Grid {
		pattern: HashMap::new(),
		max: Coord { x: 2, y: 1 }
	};
	expected.pattern.insert(Coord { x: 0, y: 0}, '.');
	expected.pattern.insert(Coord { x: 1, y: 0}, '.');
	expected.pattern.insert(Coord { x: 2, y: 0}, '#');
	expected.pattern.insert(Coord { x: 0, y: 1}, '#');
	expected.pattern.insert(Coord { x: 1, y: 1}, '.');
	expected.pattern.insert(Coord { x: 2, y: 1}, '.');

	assert_eq!(Grid::parse(input), expected);
}

fn n_trees_visited(grid: &Grid) -> u32 {
	let mut result = 0;

	for y in 0..=grid.max.y {
		if grid.tree_at(Coord { x: 3*y, y }) {
			result += 1;
		}
	}

	result
}

#[test]
fn test_n_trees_visited() {
	let grid = Grid::parse("..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#");
	assert_eq!(n_trees_visited(&grid), 7);
}
