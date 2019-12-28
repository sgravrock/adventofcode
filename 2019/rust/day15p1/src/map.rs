use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Cell {
	Empty,
	Wall,
	Oxygen
}

pub type Coord = (i32, i32);

#[derive(Debug, PartialEq)]
pub struct Map {
	pub grid: HashMap<Coord, Cell>,
	pub xrange: Range<i32>,
	pub yrange: Range<i32>,
}
