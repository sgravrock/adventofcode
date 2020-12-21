use std::collections::HashMap;
use std::ops::{Index, IndexMut};
mod input;

fn main() {
	println!("{}", solve(input::puzzle_input()));
	// 2346881602152
}

fn solve(input: &str) -> u64 {
	let mut machine = Machine::new();
	machine.run(input);
	machine.mem.words.values().sum()
}

#[derive(Debug)]
struct Machine {
	mask: Mask,
	mem: Mem
}

impl Machine {
	fn new() -> Machine {
		Machine { mask: Mask::new(), mem: Mem::new() }
	}

	fn run(&mut self, program: &str) {
		for line in program.lines() {
			let mut chunks = line.split(" = ");
			let lhs = chunks.next().unwrap();
			let rhs = chunks.next().unwrap();

			if lhs == "mask" {
				self.mask = Mask::parse(rhs);
			} else {
				let addr: u64 = lhs.replace("mem[", "")
					.replace("]", "")
					.parse().unwrap();
				let value: u64 = rhs.parse().unwrap();
				self.mem[addr] = self.mask.apply_to(value);
			}
		}
	}
}

#[derive(Debug)]
struct Mem {
	words: HashMap<u64, u64>,
}

impl Mem {
	fn new() -> Mem {
		Mem { words: HashMap::new() }
	}
}

impl Index<u64> for Mem {
	type Output = u64;

	fn index(&self, i: u64) -> &Self::Output {
		self.words.get(&i).unwrap_or(&0)
	}
}

impl IndexMut<u64> for Mem {
	fn index_mut(&mut self, i: u64) -> &mut Self::Output {
		self.words.entry(i).or_insert(0)
	}
}


#[derive(PartialEq, Debug)]
struct Mask {
	bits: (u64, u64)
}

impl Mask {
	fn new() -> Mask {
		Mask { bits: (0, 0) }
	}

	fn parse(input: &str) -> Mask {
		let mut bits = (0, 0);
	
		for (i, c) in input.chars().enumerate() {
			match c {
				'0' => bits.0 |= 1 << (35 - i),
				'1' => bits.1 |= 1 << (35 - i),
				'X' => {},
				_ => panic!("Unexpected '{}' in mask", c)
			};
		}
	
		Mask { bits }
	}

	fn apply_to(&self, value: u64) -> u64 {
		(value & !self.bits.0) | self.bits.1
	}
}

#[test]
fn test_solve() {
	let input = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0";
	assert_eq!(solve(input), 165);
}

#[test]
fn test_mask_parse() {
	assert_eq!(
		Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX10"),
		Mask { bits: (1, 2) }
	);
}

#[test]
fn test_mask_apply() {
	let mask = Mask::parse("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X");
	assert_eq!(mask.apply_to(11), 73);
}
