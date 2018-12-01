#![feature(slice_patterns)]

fn main() {
	let instructions = parse_input(INPUT);
	println!("{}", scramble("abcdefgh", instructions));
}

#[derive(PartialEq, Eq, Debug, Clone)]
enum Instr {
	SwapLetter(char, char),
	SwapPos(usize, usize),
	Reverse(usize, usize),
	RotateAbs { dir: Dir, offset: usize },
	RotateRel(char),
	Move { src: usize, dest: usize },
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum Dir { L, R }

impl Instr {
	fn parse(input: &str) -> Instr {
		let tokens: Vec<&str> = input.split(' ').collect();

		match &tokens[..] {
			&["swap", "letter", a, "with", "letter", b] =>
				Instr::SwapLetter(require_char(a), require_char(b)),

			&["swap", "position", a, "with", "position", b] =>
				Instr::SwapPos(require_num(a), require_num(b)),

			&["reverse", "positions", a, "through", b] =>
				Instr::Reverse(require_num(a), require_num(b)),

			&["rotate", "left", n, _] =>
				Instr::RotateAbs { dir: Dir::L, offset: require_num(n) },

			&["rotate", "right", n, _] =>
				Instr::RotateAbs { dir: Dir::R, offset: require_num(n) },

			&["rotate", "based", "on", "position", "of", "letter", c] =>
				Instr::RotateRel(require_char(c)),

			&["move", "position", a, "to", "position", b] =>
				Instr::Move { src: require_num(a), dest: require_num(b) },

			_ => panic!(format!("Can't parse this: {}", input))
		}
	}

	fn apply(&self, input: &str) -> String {
		match self {
			&Instr::SwapLetter(a, b) => {
				input.chars()
					.map(|c| {
						if c == a {
							b
						} else if c == b {
							a
						} else {
							c
						}
					})
					.collect()
			},
			&Instr::SwapPos(a, b) => {
				let mut chars: Vec<char> = input.chars().collect();
				let tmp = chars[a];
				chars[a] = chars[b];
				chars[b] = tmp;
				chars.into_iter().collect()
			},
			&Instr::Reverse(start, end) => {
				let prefix: String = input.chars().take(start).collect();
				let mut middle = input.chars()
					.skip(start)
					.take(end - start + 1);
				let reversed: String = reverse(&mut middle).collect();
				let suffix: String = input.chars().skip(end + 1).collect();
				format!("{}{}{}", prefix, reversed, suffix)
			},
			&Instr::RotateAbs { dir, offset } => {
				match dir {
					Dir::L => {
						let suffix: String = input.chars().take(offset).collect();
						let prefix: String = input.chars().skip(offset).collect();
						format!("{}{}", prefix, suffix)
					},
					Dir::R => {
						let n = input.chars().count() - offset;
						let prefix: String = input.chars().skip(n).collect();
						let suffix: String = input.chars().take(n).collect();
						format!("{}{}", prefix, suffix)
					},
				}
			},
			&Instr::RotateRel(c) => {
				let i = input.chars().position(|x| x == c).unwrap();
				let len = input.chars().count();
				let mut offset = 1 + i;

				if i >= 4 {
					offset += 1;
				}

				if offset % len == 0 {
					input.to_string()
				} else {
					Instr::RotateAbs { dir: Dir::R, offset: offset % len }.apply(input)
				}
			},
			&Instr::Move { src, dest } => {
				let mut chars: Vec<char> = input.chars().collect();
				let c = chars.remove(src);
				chars.insert(dest, c);
				chars.into_iter().collect()
			}
		}
	}
}

#[test]
fn test_swap_letter() {
	assert_eq!("edcba", Instr::SwapLetter('d', 'b').apply("ebcda"));
}

#[test]
fn test_swap_position() {
	assert_eq!("ebcda", Instr::SwapPos(4, 0).apply("abcde"));
}

#[test]
fn test_reverse() {
	assert_eq!("xabcdex", Instr::Reverse(1, 5).apply("xedcbax"));
	assert_eq!("xbcdeax", Instr::Reverse(1, 4).apply("xedcbax"));
}

#[test]
fn test_rotate_abs() {
	assert_eq!("bcdea", Instr::RotateAbs{dir: Dir::L, offset: 1}.apply("abcde"));
	assert_eq!("cdeab", Instr::RotateAbs{dir: Dir::L, offset: 2}.apply("abcde"));
	assert_eq!("eabcd", Instr::RotateAbs{dir: Dir::R, offset: 1}.apply("abcde"));
}

#[test]
fn test_rotate_rel() {
	assert_eq!("ecabd", Instr::RotateRel('b').apply("abdec"));
	assert_eq!("decab", Instr::RotateRel('d').apply("ecabd"));
	assert_eq!("habcdefg", Instr::RotateRel('a').apply("abcdefgh"));
	assert_eq!("ghabcdef", Instr::RotateRel('b').apply("abcdefgh"));
	assert_eq!("fghabcde", Instr::RotateRel('c').apply("abcdefgh"));
	assert_eq!("efghabcd", Instr::RotateRel('d').apply("abcdefgh"));
	assert_eq!("cdefghab", Instr::RotateRel('e').apply("abcdefgh"));
	assert_eq!("bcdefgha", Instr::RotateRel('f').apply("abcdefgh"));
	assert_eq!("abcdefgh", Instr::RotateRel('g').apply("abcdefgh"));
	assert_eq!("habcdefg", Instr::RotateRel('h').apply("abcdefgh"));
}

#[test]
fn test_move() {
	assert_eq!("bdeac", Instr::Move { src: 1, dest: 4 }.apply("bcdea"));
}

fn require_num(s: &str) -> usize {
	s.parse::<usize>().expect(&format!("Not a number: {}", s))
}

fn require_char(s: &str) -> char {
	s.chars().next().unwrap()
}

fn reverse<I: Iterator<Item = char>>(iter: &mut I) -> std::vec::IntoIter<char> {
	let mut storage: Vec<char> = vec![];

	while let Some(c) = iter.next() {
		storage.insert(0, c);
	}

	storage.into_iter()
}


fn parse_input(input: &str) -> Vec<Instr> {
	input.lines().map(Instr::parse).collect()
}

#[test]
fn test_parse_input() {
	let input = "swap letter g with letter b
swap position 6 with position 3
reverse positions 2 through 7
rotate left 6 steps
rotate right 4 steps
rotate right 1 step
rotate based on position of letter h
move position 6 to position 4";
	
	let expected = vec![
		Instr::SwapLetter('g', 'b'),
		Instr::SwapPos(6, 3),
		Instr::Reverse(2, 7),
		Instr::RotateAbs { dir: Dir::L, offset: 6 },
		Instr::RotateAbs { dir: Dir::R, offset: 4 },
		Instr::RotateAbs { dir: Dir::R, offset: 1 },
		Instr::RotateRel('h'),
		Instr::Move { src: 6, dest: 4 },
	];

	assert_eq!(expected, parse_input(input));
}

fn scramble(input: &str, instructions: Vec<Instr>) -> String {
	instructions.iter().fold(input.to_string(), |pwd, ins| ins.apply(&pwd))
}

#[test]
fn test_scramble() {
	let instructions = parse_input("swap position 4 with position 0
swap letter d with letter b
reverse positions 0 through 4
rotate left 1 step
move position 1 to position 4
move position 3 to position 0
rotate based on position of letter b
rotate based on position of letter d");
	
	assert_eq!("decab", scramble("abcde", instructions.clone()));
	assert_eq!("xbdecyza", scramble("abcdexyz", instructions));
}

static INPUT: &'static str = "move position 2 to position 6
move position 0 to position 5
move position 6 to position 4
reverse positions 3 through 7
move position 1 to position 7
swap position 6 with position 3
swap letter g with letter b
swap position 2 with position 3
move position 4 to position 3
move position 6 to position 3
swap position 4 with position 1
swap letter b with letter f
reverse positions 3 through 4
swap letter f with letter e
reverse positions 2 through 7
rotate based on position of letter h
rotate based on position of letter a
rotate based on position of letter e
rotate based on position of letter h
rotate based on position of letter c
move position 5 to position 7
swap letter a with letter d
move position 5 to position 6
swap position 4 with position 0
swap position 4 with position 6
rotate left 6 steps
rotate right 4 steps
rotate right 5 steps
swap letter f with letter e
swap position 2 with position 7
rotate based on position of letter e
move position 4 to position 5
swap position 4 with position 2
rotate right 1 step
swap letter b with letter f
rotate based on position of letter b
reverse positions 3 through 5
move position 3 to position 1
rotate based on position of letter g
swap letter c with letter e
swap position 7 with position 3
move position 0 to position 3
rotate right 6 steps
reverse positions 1 through 3
swap letter d with letter e
reverse positions 3 through 5
move position 0 to position 3
swap letter c with letter e
move position 2 to position 7
swap letter g with letter b
rotate right 0 steps
reverse positions 1 through 3
swap letter h with letter d
move position 4 to position 0
move position 6 to position 3
swap letter a with letter c
reverse positions 3 through 6
swap letter h with letter g
move position 7 to position 2
rotate based on position of letter h
swap letter b with letter h
reverse positions 2 through 6
move position 6 to position 7
rotate based on position of letter a
rotate right 7 steps
reverse positions 1 through 6
move position 1 to position 6
rotate based on position of letter g
rotate based on position of letter d
move position 0 to position 4
rotate based on position of letter e
rotate based on position of letter d
rotate based on position of letter a
rotate based on position of letter a
rotate right 4 steps
rotate based on position of letter b
reverse positions 0 through 4
move position 1 to position 7
rotate based on position of letter e
move position 1 to position 7
swap letter f with letter h
move position 5 to position 1
rotate based on position of letter f
reverse positions 0 through 1
move position 2 to position 4
rotate based on position of letter a
swap letter b with letter d
move position 6 to position 0
swap letter e with letter b
rotate right 7 steps
move position 2 to position 7
rotate left 4 steps
swap position 6 with position 1
move position 3 to position 5
rotate right 7 steps
reverse positions 0 through 6
swap position 2 with position 1
reverse positions 4 through 6
rotate based on position of letter g
move position 6 to position 4";
