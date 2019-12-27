#[derive(PartialEq, Debug, Copy, Clone)]
pub enum Direction { N, S, E, W }

#[derive(PartialEq, Debug, Copy, Clone)]
pub enum MoveResult {
	HitWall,
	Moved,
	MovedAndFoundOxygen
}

pub trait Bot {
	fn advance(&mut self, dir: Direction) -> MoveResult;
}
