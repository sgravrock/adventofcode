fn main() {
	let first: Vec<TT> = ".^.^..^......^^^^^...^^^...^...^....^^.^...^.^^^^....^...^^.^^^...^^^^.^^.^.^^..^.^^^..^^^^^^.^^^..^"
		.chars()
		.map(TT::from_char)
		.collect();
		println!("{}", num_safe(40, &first));
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum TT {
	Safe,
	Trap
}

impl TT {
	fn from_char(c: char) -> TT {
		match c {
			'.' => TT::Safe,
			'^' => TT::Trap,
			_ => panic!("Can't turn {} into a tile type", c)
		}
	}
}

fn num_safe(num_rows: usize, first_row: &[TT]) -> usize {
	build_map(num_rows, first_row)
		.iter()
		.flat_map(|row| row.iter())
		.filter(|tile| **tile == TT::Safe)
		.count()
}

#[test]
fn test_num_safe() {
	let first = vec![TT::Safe, TT::Trap, TT::Trap, TT::Safe, TT::Trap, TT::Safe, TT::Trap, TT::Trap, TT::Trap, TT::Trap];
	assert_eq!(38, num_safe(10, &first));
}

fn build_map(num_rows: usize, first_row: &[TT]) -> Vec<Vec<TT>> {
	let mut map = vec![first_row.to_vec()];

	while map.len() < num_rows {
		let prev = &(map.last().unwrap().clone());
		let next_row = build_row(prev);
		map.push(next_row);
	}

	map
}

fn build_row(prev: &[TT]) -> Vec<TT> {
	(0..prev.len()).map(|x| identify(x, prev)).collect()
}


#[test]
fn test_build_map() {
	let expected = vec![
		vec![TT::Safe, TT::Safe, TT::Trap, TT::Trap, TT::Safe, ],
		vec![TT::Safe, TT::Trap, TT::Trap, TT::Trap, TT::Trap, ],
		vec![TT::Trap, TT::Trap, TT::Safe, TT::Safe, TT::Trap, ],
	];
	assert_eq!(expected, build_map(3,
		&[TT::Safe, TT::Safe, TT::Trap, TT::Trap, TT::Safe]));
}

fn identify(x: usize, prev_row: &[TT]) -> TT {
	let prev = match x {
		0 => TT::Safe,
		_ => prev_row[x - 1]
	};
	let same = prev_row[x];
	let next = prev_row.get(x + 1).cloned().unwrap_or(TT::Safe);

	match (prev, same, next) {
		(TT::Trap, TT::Trap, TT::Safe) => TT::Trap,
		(TT::Safe, TT::Trap, TT::Trap) => TT::Trap,
		(TT::Trap, TT::Safe, TT::Safe) => TT::Trap,
		(TT::Safe, TT::Safe, TT::Trap) => TT::Trap,
		_ => TT::Safe
	}
}

#[test]
fn test_identify() {
	assert_eq!(TT::Trap, identify(1, &[TT::Trap, TT::Trap, TT::Safe]));
	assert_eq!(TT::Safe, identify(1, &[TT::Trap, TT::Trap, TT::Trap]));
	assert_eq!(TT::Trap, identify(1, &[TT::Safe, TT::Trap, TT::Trap]));
	assert_eq!(TT::Trap, identify(1, &[TT::Trap, TT::Safe, TT::Safe]));
	assert_eq!(TT::Trap, identify(1, &[TT::Safe, TT::Safe, TT::Trap]));
}

#[test]
fn test_identify_walls_are_safe() {
	assert_eq!(TT::Trap, identify(0, &[TT::Safe, TT::Trap]));
	assert_eq!(TT::Trap, identify(1, &[TT::Trap, TT::Safe]));
	assert_eq!(TT::Trap, identify(1, &[TT::Trap, TT::Trap]));
	assert_eq!(TT::Safe, identify(0, &[TT::Trap, TT::Safe]));
}
