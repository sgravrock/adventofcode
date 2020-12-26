#[macro_use]
extern crate lazy_static;
mod input;
mod grid;
mod hypercube;
use hypercube::{Hypercube, Coord};

fn main() {
	let cube = Hypercube::parse_plane(input::puzzle_input(), Cell::Inactive);
	println!("{}", solve(cube));
	// 1836
}


#[derive(PartialEq, Clone, Copy, Debug)]
enum Cell { Inactive, Active }

impl grid::FromChar for Cell {
	fn from_c(c: char) -> Cell {
		match c {
			'.' => Cell::Inactive,
			'#' => Cell::Active,
			_ => panic!("Input char '{}' is not a valid Cell", c)
		}
	}
}

impl grid::ToStr for Cell {
	fn to_s(&self) -> &'static str {
		match self {
			Cell::Inactive => ".",
			Cell::Active => "#",
		}
	}
}

fn solve(mut cube: Hypercube<Cell>) -> usize {
	for _ in 0..6 {
		cube = tick(cube);
	}

	cube.cells.values()
		.filter(|c| **c == Cell::Active)
		.count()
}

fn tick(cube: Hypercube<Cell>) -> Hypercube<Cell> {
	let xrange = cube.xrange();
	let yrange = cube.yrange();
	let zrange = cube.zrange();
	let wrange = cube.wrange();
	let mut result = Hypercube::new(Cell::Inactive);

	for x in (xrange.start() - 1)..=(xrange.end() + 1) {
		for y in (yrange.start() - 1)..=(yrange.end() + 1) {
			for z in (zrange.start() - 1)..=(zrange.end() + 1) {
				for w in (wrange.start() - 1)..=(wrange.end() + 1) {
					let c = Coord {x, y, z, w};
					let a = adj_active(&cube, c);
	
					if cube[c] == Cell::Active && (a < 2 || a > 3) {
						result[c] = Cell::Inactive;
					} else if cube[c] == Cell::Inactive && a == 3 {
						result[c] = Cell::Active;
					} else {
						result[c] = cube[c];
					}
				}
			}
		}
	}

	result
}

fn adj_active(cube: &Hypercube<Cell>, coord: Coord) -> usize {
	lazy_static! {
		static ref DELTAS: Vec<Coord> = {
			let mut deltas = Vec::new();

			for x in -1..=1 {
				for y in -1..=1 {
					for z in -1..=1 {
						for w in -1..=1 {
							if x != 0 || y != 0 || z != 0 || w != 0 {
								deltas.push(Coord {x, y, z, w});
							}
						}
					}
				}
			}

			deltas
		};
	}

	DELTAS.iter().filter(|d| cube[coord + **d] == Cell::Active).count()
}

#[test]
fn test_solve() {
	let initial = Hypercube::parse_plane("
.#.
..#
###
", Cell::Inactive);
	assert_eq!(solve(initial), 848);
}

#[test]
fn test_adj_active_single_plane() {
	let cube = Hypercube::parse_plane("
.#.
..#
###
", Cell::Inactive);
	assert_eq!(adj_active(&cube, Coord{x: 0, y: 0, z: 0, w: 0}), 1);
	assert_eq!(adj_active(&cube, Coord{x: 1, y: 0, z: 0, w: 0}), 1);
	assert_eq!(adj_active(&cube, Coord{x: 2, y: 0, z: 0, w: 0}), 2);
	assert_eq!(adj_active(&cube, Coord{x: 0, y: 1, z: 0, w: 0}), 3);
	assert_eq!(adj_active(&cube, Coord{x: 1, y: 1, z: 0, w: 0}), 5);
	assert_eq!(adj_active(&cube, Coord{x: 2, y: 1, z: 0, w: 0}), 3);
	assert_eq!(adj_active(&cube, Coord{x: 0, y: 2, z: 0, w: 0}), 1);
	assert_eq!(adj_active(&cube, Coord{x: 1, y: 2, z: 0, w: 0}), 3);
	assert_eq!(adj_active(&cube, Coord{x: 2, y: 2, z: 0, w: 0}), 2);
}
