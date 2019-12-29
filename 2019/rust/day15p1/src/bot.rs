#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Direction { N, S, E, W }

pub type Coord = (i32, i32);

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Cell {
	Empty,
	Wall,
	Oxygen
}

pub trait Bot {
	fn advance(&mut self, dir: Direction) -> Cell;
}
