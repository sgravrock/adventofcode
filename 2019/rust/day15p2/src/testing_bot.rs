use std::collections::HashMap;
use std::fmt;
use regex::Regex;
use crate::bot::{Bot, Cell, Coord, Direction};
use crate::map::Map;

#[derive(PartialEq, Clone)]
pub struct TestingBot {
	map: Map,
	pos: Coord
}

impl TestingBot {
	pub fn from_input(input: &str,  pos: Coord) -> TestingBot {
		let not_blank = Regex::new(r"[^\s]").unwrap();
		let prefix = Regex::new(r"^([\s]*\|)?").unwrap();
		let mut map = Map::new();

		let lines = input.lines()
			.filter(|line| not_blank.is_match(line))
			.map(|line| prefix.replace(line, ""))
			.enumerate();

		for (y, line) in lines {
			let chars = line.chars().enumerate();

			for (x, c) in chars {
				map.insert((x as i32, y as i32), match c {
					'#' => Cell::Wall,
					'o' => Cell::Oxygen,
					' ' => Cell::Empty,
					_ => panic!("Unexpected input char '{}'", c)
				});
			}
		}

		TestingBot { map, pos }
	}
}

impl fmt::Debug for TestingBot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "")?;
		for y in self.map.yrange.clone() {
			for x in self.map.xrange.clone() {
				let bot_here = x == self.pos.0 && y == self.pos.1;
				let cell = self.map.get_without_expanding((x, y));
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
		let cell = self.map.get(new_pos);

		if cell != Cell::Wall {
			self.pos = new_pos;
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
	assert_eq!(bot.map.get(bot.pos), Cell::Empty);
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
	assert_eq!(bot.map.get(bot.pos), Cell::Empty);
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
	assert_eq!(bot.map.get(bot.pos), Cell::Empty);
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
	assert_eq!(bot.map.get(bot.pos), Cell::Empty);
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
