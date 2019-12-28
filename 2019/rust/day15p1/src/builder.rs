use crate::map::{Map, Cell, Coord};
use crate::bot::{Bot, Direction};
use crate::testing_bot::TestingBot;
use std::collections::HashMap;

struct Builder {
	map: Map,
	// Maps from cell to a (possibly suboptimal) path to that cell.
	paths_to_cells: HashMap<Coord, Vec<Coord>>,
}

pub fn build_map<T>(mut bot: T, start: Coord) -> Map where T: Bot {
	let mut map = Map::new();
	map[start] = Cell::Empty;
	// TODO: Won't work. We need to BFS instead.
	// How do we maintain the data structure for that?
	build_map_sub(&mut map, &mut bot, start, Direction::N);
	build_map_sub(&mut map, &mut bot, start, Direction::S);
	build_map_sub(&mut map, &mut bot, start, Direction::E);
	build_map_sub(&mut map, &mut bot, start, Direction::W);
	map
}

fn build_map_sub<T>(bot: &mut T, pos: Coord, next_move: Direction, map: &mut Map) where T: Bot {
}

#[test]
fn test_build_map_simple() {
	let mut bot = TestingBot::from_input("
		|####
		|#  #
		|## #
		|#o #
		|####
	", (1, 1));

	let mut expected_grid = HashMap::new();
	expected_grid.insert((0, 0), Cell::Wall);
	expected_grid.insert((1, 0), Cell::Wall);
	expected_grid.insert((2, 0), Cell::Wall);
	expected_grid.insert((3, 0), Cell::Wall);

	expected_grid.insert((0, 1), Cell::Wall);
	expected_grid.insert((1, 1), Cell::Empty);
	expected_grid.insert((2, 1), Cell::Empty);
	expected_grid.insert((3, 1), Cell::Wall);

	expected_grid.insert((0, 2), Cell::Wall);
	expected_grid.insert((1, 2), Cell::Wall);
	expected_grid.insert((2, 2), Cell::Empty);
	expected_grid.insert((3, 2), Cell::Empty);

	expected_grid.insert((0, 3), Cell::Wall);
	expected_grid.insert((1, 3), Cell::Oxygen);
	expected_grid.insert((2, 3), Cell::Empty);
	expected_grid.insert((3, 3), Cell::Wall);

	expected_grid.insert((0, 4), Cell::Wall);
	expected_grid.insert((1, 4), Cell::Wall);
	expected_grid.insert((2, 4), Cell::Wall);
	expected_grid.insert((3, 4), Cell::Wall);

	let expected = Map {
		grid: expected_grid,
		xrange: 0..4,
		yrange: 0..5,
	};

	assert_eq!(build_map(bot, (1, 1)), expected);
}
