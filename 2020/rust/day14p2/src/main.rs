use std::collections::HashMap;
use std::ops::{Index, IndexMut};
#[cfg(test)]
use std::collections::HashSet;
#[cfg(test)]
use std::iter::FromIterator;
mod input;

fn main() {
	println!("{}", solve(input::puzzle_input()));
	// 3885232834169
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
				let base_addr: u64 = lhs.replace("mem[", "")
					.replace("]", "")
					.parse().unwrap();
				let value: u64 = rhs.parse().unwrap();

				for addr in self.mask.apply_to(base_addr) {
					self.mem[addr] = value;
				}
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
	set_bits: u64,
	floating_bits: u64,
}

impl Mask {
	fn new() -> Mask {
		Mask { set_bits: 0, floating_bits: 0 }
	}

	fn parse(input: &str) -> Mask {
		let mut set_bits = 0;
		let mut floating_bits = 0;
	
		for (i, c) in input.chars().enumerate() {
			match c {
				'0' => {},
				'1' => set_bits |= 1 << (35 - i),
				'X' => floating_bits |= 1 << (35 - i),
				_ => panic!("Unexpected '{}' in mask", c)
			};
		}
	
		assert!(set_bits & floating_bits == 0);
		Mask { set_bits, floating_bits }
	}

	fn apply_to(&self, value: u64) -> Vec<u64> {
		let known_bits = value | self.set_bits;
		let mut result = vec![known_bits];

		for i in 0..36 {
			if self.floating_bits & (1 << i)  != 0 {
				let n = result.len();

				for j in 0..n {

					if result[j] & (1 << i) == 0 {
						result.push(result[j] | (1 << i));
					} else {
						result.push(result[j] & !(1 << i));
					}
				}
			}
		}
		result
	}
}

#[test]
fn test_mask_parse() {
	assert_eq!(
		Mask::parse("00000000000000000000000000000000001X"),
		Mask { floating_bits: 1, set_bits: 2 }
	);
}

#[test]
fn test_mask_apply() {
	let mask = Mask::parse("000000000000000000000000000000X1001X");
	let expected: HashSet<u64> = HashSet::from_iter(vec![26,27, 58, 59]);
	assert_eq!(HashSet::from_iter(mask.apply_to(42)), expected);
}
