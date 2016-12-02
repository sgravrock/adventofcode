struct Movement {
	turn: char,
	dist: i32
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum Orientation {
	North,
	East,
	South,
	West,
}


struct Position {
	north: i32,
	east: i32,
	orientation: Orientation
}

fn main() {
	let input = "R2, L1, R2, R1, R1, L3, R3, L5, L5, L2, L1, R4, R1, R3, L5, L5, R3, L4, L4, R5, R4, R3, L1, L2, R5, R4, L2, R1, R4, R4, L2, L1, L1, R190, R3, L4, R52, R5, R3, L5, R3, R2, R1, L5, L5, L4, R2, L3, R3, L1, L3, R5, L3, L4, R3, R77, R3, L2, R189, R4, R2, L2, R2, L1, R5, R4, R4, R2, L2, L2, L5, L1, R1, R2, L3, L4, L5, R1, L1, L2, L2, R2, L3, R3, L4, L1, L5, L4, L4, R3, R5, L2, R4, R5, R3, L2, L2, L4, L2, R2, L5, L4, R3, R1, L2, R2, R4, L1, L4, L4, L2, R2, L4, L1, L1, R4, L1, L3, L2, L2, L5, R5, R2, R5, L1, L5, R2, R4, R4, L2, R5, L5, R5, R5, L4, R2, R1, R1, R3, L3, L3, L4, L3, L2, L2, L2, R2, L1, L3, R2, R5, R5, L4, R3, L3, L4, R2, L5, R5";
	println!("{}", distance(parse_input(input)));
}

fn parse_input(input: &str) -> Vec<Movement> {
	input.split(", ")
		.map(|s| {
			Movement {
				turn: first(s),
				dist: to_int(s)
			}
		})
		.collect()
}

#[test]
fn test_parse_input() {
	let result = parse_input("R1, L3");
	assert_eq!(2, result.len());
	assert_eq!('L', result[1].turn);
	assert_eq!(3, result[1].dist);
}

fn first(s: &str) -> char {
	s.chars().next().unwrap()
}

fn to_int(s: &str) -> i32 {
	let mut result = 0;

	for c in s.chars().skip(1) {
		let n = c as i32 - 48;
		result = result * 10 + n;
	}

	result
}

fn distance(movements: Vec<Movement>) -> i32 {
	let start = Position { north: 0, east: 0, orientation: Orientation::North };
	let dest = movements.iter().fold(start, make_move);
	dest.north.abs() + dest.east.abs()
}

fn make_move(start: Position, m: &Movement) -> Position {
	let o = next_orientation(start.orientation, m.turn);

	match o {
		Orientation::North => Position {
			north: start.north + m.dist,
			east: start.east,
			orientation: o
		},
		Orientation::South => Position {
			north: start.north - m.dist,
			east: start.east,
			orientation: o
		},
		Orientation::East => Position {
			north: start.north,
			east: start.east + m.dist,
			orientation: o
		},
		Orientation::West => Position {
			north: start.north,
			east: start.east - m.dist,
			orientation: o
		},
	}
}

#[test]
fn test_make_move() {
	let result = make_move(
		Position { north: 0, east: 0, orientation: Orientation::East },
		&Movement { turn: 'R', dist: 2 });
	assert_eq!(Orientation::South, result.orientation);
	assert_eq!(0, result.east);
	assert_eq!(-2, result.north);
}

fn next_orientation(start: Orientation, turn: char) -> Orientation {
	let n = o_to_i(start);

	let n2 = match turn {
		'R' => n + 1,
		'L' => n - 1,
		_ => panic!("Unrecognized turn: {}", turn)
	};
	i_to_o(nomalize_orientation_num(n2)).unwrap()
}

fn nomalize_orientation_num(i: i32) -> i32 {
	match i {
		-1 => 3,
		4 => 0,
		_ => i
	}
}

fn o_to_i(o: Orientation) -> i32 {
	match o {
		Orientation::North => 0,
		Orientation::East => 1,
		Orientation::South => 2,
		Orientation::West => 3,
	}
}

fn i_to_o(i: i32) -> Option<Orientation> {
	match i {
		0 => Some(Orientation::North),
		1 => Some(Orientation::East),
		2 => Some(Orientation::South),
		3 => Some(Orientation::West),
		_ => None
	}
}

#[test]

fn test_next_orientation() {
	assert_eq!(Orientation::East, next_orientation(Orientation::North, 'R'));
	assert_eq!(Orientation::West, next_orientation(Orientation::North, 'L'));
	assert_eq!(Orientation::South, next_orientation(Orientation::East, 'R'));
	assert_eq!(Orientation::North, next_orientation(Orientation::East, 'L'));
	assert_eq!(Orientation::West, next_orientation(Orientation::South, 'R'));
	assert_eq!(Orientation::East, next_orientation(Orientation::South, 'L'));
	assert_eq!(Orientation::North, next_orientation(Orientation::West, 'R'));
	assert_eq!(Orientation::South, next_orientation(Orientation::West, 'L'));
}
