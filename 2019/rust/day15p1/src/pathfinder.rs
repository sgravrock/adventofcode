use crate::bot::{Bot, Cell, Coord, Direction};
#[cfg(test)] use crate::testing_bot::TestingBot;
use std::collections::{VecDeque, HashSet};

struct Path {
	moves: Vec<Direction>,
	pos: Coord
}

impl Path {
	fn append(&self, dir: Direction) -> Path {
		let next_pos = match dir {
			Direction::N => (self.pos.0, self.pos.1 - 1),
			Direction::S => (self.pos.0, self.pos.1 + 1),
			Direction::E => (self.pos.0 + 1, self.pos.1),
			Direction::W => (self.pos.0 - 1, self.pos.1),
		};
		let mut next_moves = self.moves.clone();
		next_moves.push(dir);
		Path { moves: next_moves, pos: next_pos }
	}
}

pub fn shortest_path_len<T>(
	bot: T,
	start: Coord
) -> usize where T: Bot, T: Clone {
	let mut visited: HashSet<Coord> = HashSet::new();
	visited.insert(start);
	let mut todo: VecDeque<Path> = VecDeque::new();
	enqueue_neighbors(&mut todo, &visited, Path { moves: vec![], pos: start });

	loop {
		let path = todo.pop_front().unwrap_or_else(|| {
			panic!("Didn't find the oxygen generator");
		});

		match move_bot(bot.clone(), &path) {
			Cell::Oxygen => return path.moves.len(),
			Cell::Wall => {},
			Cell::Empty => {
				visited.insert(path.pos);
				enqueue_neighbors(&mut todo, &visited, path);
			}
		}
	}
}

static DIRECTIONS: [Direction; 4] = [Direction::N, Direction::S, Direction::E, Direction::W];

fn enqueue_neighbors(queue: &mut VecDeque<Path>, visited: &HashSet<Coord>, path: Path) {
	for dir in DIRECTIONS.iter() {
		let next_path = path.append(*dir);

		if !visited.contains(&next_path.pos) {
			queue.push_back(next_path);
		}
	}
}


fn move_bot<T>(mut bot: T, path: &Path) -> Cell where T: Bot {
	for i in 0..path.moves.len() {
		let move_result = bot.advance(path.moves[i]);

		if i + 1 == path.moves.len() {
			return move_result;
		}
		else if move_result != Cell::Empty {
			panic!("Got a {:?} before the last move", move_result);
		}
	}

	panic!("Can't happen");
}

#[test]
fn test_find_o2_simple() {
	let bot = TestingBot::from_input("
		|####
		|#  #
		|## #
		|#o #
		|####
	", (1, 1));

	assert_eq!(shortest_path_len(bot, (1, 1)), 4);
}

#[test]
fn test_find_o2_dead_end() {
	let bot = TestingBot::from_input("
		|###
		|# ##
		|#  #
		|## #
		|#o #
		|####
	", (1, 2));

	assert_eq!(shortest_path_len(bot, (1, 2)), 4);
}

#[test]
fn test_find_o2_unbounded() {
	let bot = TestingBot::from_input("
		|
		| # 
		| o 
	", (1, 0));

	assert_eq!(shortest_path_len(bot, (1, 0)), 4);
}
