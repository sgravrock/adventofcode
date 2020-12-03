mod input;
use std::collections::HashMap;

fn main() {
    println!("{}", solve(&Grid::parse(input::puzzle_input())));
	// 2421944712
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
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
		match self.pattern.get(&adjusted) {
			Some(c) => *c == '#',
			None => false
		}
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

fn n_trees_visited(grid: &Grid, slope: Coord) -> u32 {
	let mut result = 0;
	let mut pos = Coord { x: 0, y: 0 };

	while pos.y <= grid.max.y {
		pos.x += slope.x;
		pos.y += slope.y;

		if grid.tree_at(pos) {
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
	assert_eq!(n_trees_visited(&grid, Coord { x: 1, y: 1 }), 2);
	assert_eq!(n_trees_visited(&grid, Coord { x: 3, y: 1 }), 7);
	assert_eq!(n_trees_visited(&grid, Coord { x: 5, y: 1 }), 3);
	assert_eq!(n_trees_visited(&grid, Coord { x: 7, y: 1 }), 4);
	assert_eq!(n_trees_visited(&grid, Coord { x: 1, y: 2 }), 2);
}

fn solve(grid: &Grid) -> u32 {
	static SLOPES: &[Coord; 5] = &[
		Coord { x: 1, y: 1 },
		Coord { x: 3, y: 1 },
		Coord { x: 5, y: 1 },
		Coord { x: 7, y: 1 },
		Coord { x: 1, y: 2 },
	];

	SLOPES.iter()
		.map(|s| n_trees_visited(&grid, *s))
		.fold(1, |acc, n| acc * n)
}

#[test]
fn test_solve() {
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
	assert_eq!(solve(&grid), 336);
}
