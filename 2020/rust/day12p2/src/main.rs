mod input;
use std::ops::{AddAssign, Mul};

fn main() {
    println!("{}", solve(input::puzzle_input()));
	 // 46530
}

#[derive(PartialEq, Debug, Clone, Copy)]
struct Coord {
	x: i32,
	y: i32,
}

impl Coord {
	fn rotate(self: &Coord, degrees: i32) -> Coord {
		match normalize_angle(degrees) {
			90 => Coord { x: self.y, y: -1 * self.x },
			180 => Coord { x: -1 * self.x, y: -1 * self.y },
			270 => Coord { x: -1 * self.y, y: self.x },
			_ => panic!("Don't know how to rotate {} yet", degrees)
		}
	}
}

impl AddAssign<Coord> for Coord {
	fn add_assign(&mut self, other: Self) {
		self.x += other.x;
		self.y += other.y;
	}
}

impl Mul<i32> for Coord {
	type Output = Self;

	fn mul(self, rhs: i32) -> Self {
		Coord { x: self.x * rhs, y: self.y * rhs }
	}
}

#[derive(PartialEq, Debug, Clone, Copy)]
struct State {
	pos: Coord,
	waypoint: Coord,
}

fn solve(input: &str) -> i32 {
	let initial_state = State {
		pos: Coord { x: 0, y: 0 },
		waypoint: Coord { x: 10, y: 1 },
	};

	let end_state = input.lines().fold(initial_state, handle_instruction);
	end_state.pos.x.abs() + end_state.pos.y.abs()
}

fn handle_instruction(mut state: State, instruction: &str) -> State {
	let mut chars = instruction.chars();
	let cmd = chars.next().unwrap();
	let val = chars.collect::<String>().parse::<i32>().unwrap();

	match cmd {
		'N' => state.waypoint.y += val,
		'S' => state.waypoint.y -= val,
		'E' => state.waypoint.x += val,
		'W' => state.waypoint.x -= val,
		'F' => state.pos += state.waypoint * val,
		'L' => state.waypoint = state.waypoint.rotate(-1 * val),
		'R' => state.waypoint = state.waypoint.rotate(val),
		_ => panic!("Don't know how to '{}'", cmd)
	}

	state
}

fn normalize_angle(degrees: i32) -> i32 {
	let wrapped = degrees % 360;
	if wrapped < 0 {
		360 + wrapped
	} else {
		wrapped
	}
}

#[test]
fn test_solve() {
	let input = "F10
N3
F7
R90
F11";
	assert_eq!(solve(input), 286);
}

#[test]
fn test_handle_instruction_f() {
	let start = State {
		pos: Coord { x: 0, y: 0 },
		waypoint: Coord { x: 10, y: 1 },
	};
	assert_eq!(handle_instruction(start, "F10"), State {
		pos: Coord { x: 100, y: 10 },
		waypoint: start.waypoint,
	});
}

#[test]
fn test_handle_instruction_n() {
	let start = State {
		pos: Coord { x: 100, y: 10 },
		waypoint: Coord { x: 10, y: 1 },
	};
	assert_eq!(handle_instruction(start, "N3"), State {
		pos: start.pos,
		waypoint: Coord { x: 10, y: 4 },
	});
}

#[test]
fn test_handle_instruction_r() {
	let start = State {
		pos: Coord { x: 170, y: 38 },
		waypoint: Coord { x: 10, y: 4 },
	};
	assert_eq!(handle_instruction(start, "R90"), State {
		pos: start.pos,
		waypoint: Coord { x: 4, y: -10 },
	});
	assert_eq!(handle_instruction(start, "R180"), State {
		pos: start.pos,
		waypoint: Coord { x: -10, y: -4 },
	});
}

#[test]
fn test_handle_instruction_l() {
	let start = State {
		pos: Coord { x: 170, y: 38 },
		waypoint: Coord { x: 10, y: 4 },
	};
	assert_eq!(handle_instruction(start, "L90"), State {
		pos: start.pos,
		waypoint: Coord { x: -4, y: 10 },
	});
	assert_eq!(handle_instruction(start, "L180"), State {
		pos: start.pos,
		waypoint: Coord { x: -10, y: -4 },
	});
}
