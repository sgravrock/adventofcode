use std::collections::HashMap;
use std::fmt;
use std::ops::Range;
use std::cmp::{min, max};
use regex::Regex;
use crate::bot::{Bot, Cell, Coord, Direction};

#[derive(Debug, PartialEq, Clone)]
struct Map {
	pub grid: HashMap<Coord, Cell>,
	pub xrange: Range<i32>,
	pub yrange: Range<i32>,
}


#[derive(PartialEq, Clone)]
pub struct TestingBot {
	map: Map,
	pos: Coord
}

impl TestingBot {
	pub fn from_input(input: &str,  pos: Coord) -> TestingBot {
		let not_blank = Regex::new(r"[^\s]").unwrap();
		let prefix = Regex::new(r"^([\s]*\|)?").unwrap();
		let mut grid: HashMap<Coord, Cell> = HashMap::new();
		let mut xmin = 0;
		let mut xmax = 0;
		let mut ymin = 0;
		let mut ymax = 0;

		let lines = input.lines()
			.filter(|line| not_blank.is_match(line))
			.map(|line| prefix.replace(line, ""))
			.enumerate();

		for (y, line) in lines {
			let chars = line.chars().enumerate();

			for (x, c) in chars {
				grid.insert((x as i32, y as i32), match c {
					'#' => Cell::Wall,
					'o' => Cell::Oxygen,
					' ' => Cell::Empty,
					_ => panic!("Unexpected input char '{}'", c)
				});
				xmin = min(xmin, x as i32);
				xmax = max(xmax, x as i32);
				ymin = min(ymin, y as i32);
				ymax = max(ymax, y as i32);
			}
		}

		TestingBot {
			map: Map {
				grid,
				xrange: xmin..(xmax + 1),
				yrange: ymin..(ymax + 1),
			},
			pos
		}
	}

	fn expand_ranges(&mut self) {
		self.map.xrange = 
			min(self.map.xrange.start, self.pos.0)
			..
			max(self.map.xrange.end, self.pos.0 + 1);
		self.map.yrange = 
			min(self.map.yrange.start, self.pos.1)
			..
			max(self.map.yrange.end, self.pos.1 + 1);
	}
}

impl fmt::Debug for TestingBot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "")?;
		for y in self.map.yrange.clone() {
			for x in self.map.xrange.clone() {
				let bot_here = x == self.pos.0 && y == self.pos.1;
				let cell = self.map.grid.get(&(x, y)).unwrap_or(&Cell::Empty);
				write!(f, "{}", match cell {
					Cell::Empty => if bot_here { 'x' } else { ' ' },
					Cell::Oxygen => if bot_here { '!' } else { 'o' },
					Cell::Wall => '#',
				})?;
			}

			writeln!(f, "")?;
		}

		Ok(())
	}
}


impl Bot for TestingBot {
	fn advance(&mut self, dir: Direction) -> Cell {
		let delta = match dir {
			Direction::N => (0, -1),
			Direction::S => (0, 1),
			Direction::E => (-1, 0),
			Direction::W => (1, 0),
		};
		let new_pos = (self.pos.0 + delta.0, self.pos.1 + delta.1);
		let cell = *(self.map.grid.entry(new_pos).or_insert(Cell::Empty));

		if cell != Cell::Wall {
			self.pos = new_pos;
			self.expand_ranges();
		}

		cell
	}
}

#[test]
fn test_advance_n() {
	let mut bot = TestingBot::from_input("
		| 
		| 
	", (0, 1));
	assert_eq!(bot.advance(Direction::N), Cell::Empty);
	assert_eq!(bot.pos, (0, 0));
	assert_eq!(bot.advance(Direction::N), Cell::Empty);
	assert_eq!(bot.pos, (0, -1));
	assert_eq!(bot.map.grid[&bot.pos], Cell::Empty);
	assert_eq!(bot.map.yrange.start, -1);
	assert_eq!(format!("{:?}", bot), "\nx\n \n \n");
}

#[test]
fn test_advance_s() {
	let mut bot = TestingBot::from_input("
		| 
		| 
	", (0, 0));
	assert_eq!(bot.advance(Direction::S), Cell::Empty);
	assert_eq!(bot.pos, (0, 1));
	assert_eq!(bot.advance(Direction::S), Cell::Empty);
	assert_eq!(bot.pos, (0, 2));
	assert_eq!(bot.map.grid[&bot.pos], Cell::Empty);
	assert_eq!(bot.map.yrange.end, 3);
	assert_eq!(format!("{:?}", bot), "\n \n \nx\n");
}

#[test]
fn test_advance_e() {
	let mut bot = TestingBot::from_input("|  ", (1, 0));
	assert_eq!(bot.advance(Direction::E), Cell::Empty);
	assert_eq!(bot.pos, (0, 0));
	assert_eq!(bot.advance(Direction::E), Cell::Empty);
	assert_eq!(bot.pos, (-1, 0));
	assert_eq!(bot.map.grid[&bot.pos], Cell::Empty);
	assert_eq!(bot.map.xrange.start, -1);
	assert_eq!(format!("{:?}", bot), "\nx  \n");
}

#[test]
fn test_advance_w() {
	let mut bot = TestingBot::from_input("|  ", (0, 0));
	assert_eq!(bot.advance(Direction::W), Cell::Empty);
	assert_eq!(bot.pos, (1, 0));
	assert_eq!(bot.advance(Direction::W), Cell::Empty);
	assert_eq!(bot.pos, (2, 0));
	assert_eq!(bot.map.grid[&bot.pos], Cell::Empty);
	assert_eq!(bot.map.xrange.end, 3);
	assert_eq!(format!("{:?}", bot), "\n  x\n");
}

#[test]
fn test_advance_into_wall() {
	let mut bot = TestingBot::from_input(" #", (0, 0));
	assert_eq!(bot.advance(Direction::W), Cell::Wall);
	assert_eq!(bot.pos, (0, 0));
}

#[test]
fn test_advance_into_oxygen() {
	let mut bot = TestingBot::from_input(" o", (0, 0));
	assert_eq!(bot.advance(Direction::W), Cell::Oxygen);
	assert_eq!(bot.pos, (1, 0));
}


#[test]
fn test_from_input() {
	let input = "
		|###
		|#o#
		|# #
		|###
	";
	let starting_pos = (1, 2);
	let mut expected_grid = HashMap::new();
	expected_grid.insert((0, 0), Cell::Wall);
	expected_grid.insert((1, 0), Cell::Wall);
	expected_grid.insert((2, 0), Cell::Wall);

	expected_grid.insert((0, 1), Cell::Wall);
	expected_grid.insert((1, 1), Cell::Oxygen);
	expected_grid.insert((2, 1), Cell::Wall);

	expected_grid.insert((0, 2), Cell::Wall);
	expected_grid.insert((1, 2), Cell::Empty);
	expected_grid.insert((2, 2), Cell::Wall);

	expected_grid.insert((0, 3), Cell::Wall);
	expected_grid.insert((1, 3), Cell::Wall);
	expected_grid.insert((2, 3), Cell::Wall);

	assert_eq!(
		TestingBot::from_input(input, starting_pos),
		TestingBot {
			map: Map {
				grid: expected_grid,
				xrange: 0..3,
				yrange: 0..4,
			},
			pos: starting_pos
		}
	);
}

#[test]
fn test_debug_matches_from_input() {
	let bot = TestingBot::from_input(
		"
			|###
			|#o#
			|# #
			|###
		",
		(1, 2)
	);
	let expected = "
###
#o#
#x#
###
";
	assert_eq!(format!("{:?}", bot), expected);
}

#[test]
fn test_debug_matches_bot_at_o2() {
	let bot = TestingBot::from_input(
		"
			|###
			|#o#
			|# #
			|###
		",
		(1, 1)
	);
	let expected = "
###
#!#
# #
###
";
	assert_eq!(format!("{:?}", bot), expected);
}
