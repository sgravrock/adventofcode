#[macro_use] extern crate lazy_static;
extern crate regex;
use std::collections::HashMap;
use std::collections::VecDeque;
use regex::Regex;

fn main() {
	let input =
"Begin in state A.
Perform a diagnostic checksum after 12667664 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state C.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state D.

In state C:
  If the current value is 0:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state E.

In state D:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the right.
    - Continue with state B.

In state E:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state F.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state C.

In state F:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state D.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.";

	let program = parse_program(input);
	let mut machine = Machine::new();
	let checksum = machine.compute_checksum(&program);
	println!("Puzzle solution is {}.", checksum);
}

struct Machine {
	state: char,
	tape: InfiniteTape,
	pos: isize
}

impl Machine {
	fn new() -> Machine {
		Machine { state: 'A', tape: InfiniteTape::new(), pos: 0 }
	}

	fn compute_checksum(&mut self, program: &Program) -> usize {
		for _ in 0..program.checksum_after_steps {
			self.step(&program);
		}

		self.tape.iter()
			.filter(|x| **x)
			.count()
	}

	fn step(&mut self, program: &Program) {
		let instruction = match self.tape.get(self.pos) {
			true => &program.instructions[&self.state].1,
			false => &program.instructions[&self.state].0,
		};
		self.tape.set(self.pos, instruction.value_to_write);
		self.pos += instruction.dir;
		self.state = instruction.next_state;
	}
}

struct InfiniteTape {
	data: VecDeque<bool>,
	offset: isize
}

impl InfiniteTape {
	fn new() -> InfiniteTape {
		let mut data = VecDeque::new();
		data.push_back(false);
		InfiniteTape { data, offset: 0 }
	}

	fn iter(&self) -> std::collections::vec_deque::Iter<bool> {
		self.data.iter()
	}

	fn get(&mut self, pos: isize) -> bool {
		self.zero_fill(pos);
		self.data[(self.offset + pos) as usize]
	}

	fn set(&mut self, pos: isize, value: bool) {
		self.zero_fill(pos);
		self.data[(self.offset + pos) as usize] = value;
	}

	fn zero_fill(&mut self, pos: isize) {
		while self.offset + pos < 0 {
			self.data.push_front(false);
			self.offset += 1;
		}

		while (self.offset + pos) as usize >= self.data.len() {
			self.data.push_back(false);
		}
	}
}

#[derive(Debug, Eq, PartialEq)]
struct Program {
	instructions: HashMap<char, (Instruction, Instruction)>,
	checksum_after_steps: u32
}

#[derive(Debug, Eq, PartialEq)]
struct Instruction {
	value_to_write: bool,
	dir: isize,
	next_state: char
}

struct LineReader<'a> {
	lines: Vec<&'a str>,
	next_i: usize
}

impl<'a> LineReader<'a> {
	fn new(lines: Vec<&str>) -> LineReader {
		LineReader { lines: lines, next_i: 0 }
	}

	fn has_more(&self) -> bool {
		self.next_i < self.lines.len()
	}

	fn next(&mut self) -> &str {
		let result = self.lines[self.next_i];
		self.next_i += 1;
		result
	}
}

struct ProgramReader<'a> {
	line_reader: LineReader<'a>
}

impl <'a> ProgramReader<'a> {
	fn new(line_reader: LineReader) -> ProgramReader {
		ProgramReader { line_reader: line_reader }
	}

	fn read_program(&mut self) -> Program {
		self.read_exact("Begin in state A.");
		let checksum_re = Regex::new("Perform a diagnostic checksum after ([0-9]+) steps.$").unwrap();
		let checksum_after_steps = self.read_re(&checksum_re)
			.parse::<u32>()
			.unwrap();

		let mut states = HashMap::new();

		while self.has_more() {
			let (state, instructions) = self.read_state();
			states.insert(state, instructions);
		}

		Program {
			instructions: states,
			checksum_after_steps: checksum_after_steps
		}
	}

	fn read_state(&mut self) -> (char, (Instruction, Instruction)) {
		self.read_exact("");
		lazy_static! {
			static ref re: Regex = Regex::new(r"^In state ([A-Z]):$").unwrap();
		}
		let state = self.read_re(&re)
			.chars().next().unwrap();
		let i0 = self.read_action(0);
		let i1 = self.read_action(1);
		(state, (i0, i1))
	}

	fn read_action(&mut self, expected_current_value: u32) -> Instruction {
		lazy_static! {
			static ref write_re: Regex = Regex::new(r"^    - Write the value ([01])\.$").unwrap();
			static ref move_re: Regex = Regex::new(r"^    - Move one slot to the (right|left)\.").unwrap();
			static ref next_state_re: Regex = Regex::new(r"^    - Continue with state ([A-z]).$").unwrap();
		}

		self.read_exact(&format!("  If the current value is {}:", expected_current_value));
		let value_to_write = self.read_re(&write_re)
			.chars().next().unwrap() == '1';

		let dir_s = self.read_re(&move_re);
		let dir = if dir_s == "left" { -1 } else { 1 };

		let next_state = self.read_re(&next_state_re)
			.chars().next().unwrap();

		Instruction { value_to_write, dir, next_state }
	}

	fn has_more(&self) -> bool {
		self.line_reader.has_more()
	}

	fn read_exact(&mut self, expected: &str) {
		let line = self.line_reader.next();
		
		if line != expected {
			panic!("Expected to read '{}' but got '{}'", expected, line);
		}
	}

	fn read_re(&mut self, re: &Regex) -> &str {
		let line = self.line_reader.next();

		re.captures(line)
			.unwrap_or_else(|| {
				panic!("Expected to read '{}' but got '{}'", re, line);
			})
			.get(1).unwrap()
			.as_str()
	}
}


fn parse_program(input: &str) -> Program {
	let lines = input.split("\n").collect::<Vec<&str>>();
	let mut reader = ProgramReader::new(LineReader::new(lines));
	reader.read_program()
}

#[test]
fn test_parse_program() {
	let input =
"Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.";

	let mut expected = Program {
		instructions: HashMap::new(),
		checksum_after_steps: 6
	};
	expected.instructions.insert('A', (
		Instruction {
			value_to_write: true,
			dir: 1,
			next_state: 'B'
		},
		Instruction {
			value_to_write: false,
			dir: -1,
			next_state: 'B'
		},
	));
	expected.instructions.insert('B', (
		Instruction {
			value_to_write: true,
			dir: -1,
			next_state: 'A'
		},
		Instruction {
			value_to_write: true,
			dir: 1,
			next_state: 'A'
		},
	));

	assert_eq!(parse_program(input), expected);
}

#[test]
fn test_compute_checksum() {
	let input =
"Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.";

	let program = parse_program(input);
	let mut machine = Machine::new();
	let checksum = machine.compute_checksum(&program);
	assert_eq!(3, checksum);
}
