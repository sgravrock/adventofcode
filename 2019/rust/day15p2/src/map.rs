use std::collections::HashMap;
use std::ops::Range;
use std::cmp::{min, max};
use std::fmt;
use crate::bot::{Cell, Coord};

#[derive(PartialEq, Clone)]
pub struct Map {
	pub grid: HashMap<Coord, Cell>,
	pub xrange: Range<i32>,
	pub yrange: Range<i32>,
}

impl Map {
	pub fn new() -> Map {
		Map {
			grid: HashMap::new(),
			xrange: 0..0,
			yrange: 0..0
		}
	}

	pub fn get(&mut self, coord: Coord) -> Cell {
		self.expand_ranges(coord);
		self.get_without_expanding(coord)
	}

	pub fn get_without_expanding(&self, coord: Coord) -> Cell {
		*self.grid.get(&coord).unwrap_or(&Cell::Empty)
	}

	pub fn insert(&mut self, coord: Coord, cell: Cell) {
		self.grid.insert(coord, cell);
		self.expand_ranges(coord);
	}

	fn expand_ranges(&mut self, coord: Coord) {
		self.xrange = 
			min(self.xrange.start, coord.0)
			..
			max(self.xrange.end, coord.0 + 1);
		self.yrange = 
			min(self.yrange.start, coord.1)
			..
			max(self.yrange.end, coord.1 + 1);
	
	}
}

impl fmt::Debug for Map {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "")?;
		for y in self.yrange.clone() {
			for x in self.xrange.clone() {
				write!(f, "{}", match self.get_without_expanding((x, y)) {
					Cell::Empty => ' ',
					Cell::Oxygen => 'o',
					Cell::Wall => '#',
				})?;
			}

			writeln!(f, "")?;
		}

		Ok(())
	}
}
