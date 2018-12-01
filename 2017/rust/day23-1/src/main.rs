use std::collections::HashMap;

fn main() {
	let input =
"set b 99
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23";
	let mut processor = Processor::new();
	processor.execute(Instruction::parse(input));
	println!("Puzzle solution is {}.", processor.num_muls);
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Operand {
	Value(i32),
	Ref(char)
}

impl Operand {
	fn parse(s: &str) -> Option<Operand> {
		match s.parse::<i32>() {
			Ok(n) => Some(Operand::Value(n)),
			Err(_) => s.chars().next().map(|c| Operand::Ref(c))
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum Instruction {
	Set(char, Operand),
	Sub(char, Operand),
	Mul(char, Operand),
	Jnz(Operand, Operand),
}

impl Instruction {
	fn parse(input: &str) -> Vec<Instruction> {
		input.split("\n")
			.map(|line| {
				let tokens = line.split(" ").collect::<Vec<&str>>();
				if tokens.len() != 3 {
					panic!("Parse error: {}", line);
				}
				let reg = tokens[1].chars().next()
					.unwrap_or_else(|| panic!("Parse error: {}", line));
				let operand = Operand::parse(tokens[2])
					.unwrap_or_else(|| panic!("Parse error: {}", line));

				match tokens[0] {
					"set" => Instruction::Set(reg, operand),
					"sub" => Instruction::Sub(reg, operand),
					"mul" => Instruction::Mul(reg, operand),
					"jnz" => {
						let cond = Operand::parse(tokens[1])
							.unwrap_or_else(|| panic!("Parse error: {}", line));
						Instruction::Jnz(cond, operand)
					}
					_ => panic!("Parse error: {}", line)
				}
			})
			.collect()
	}
}

#[test]
fn test_instruction_parse() {
	let input = "set b 99
set b a
jnz g 5
jnz 1 5
mul b 100
sub b -100000";
	let expected = vec![
		Instruction::Set('b', Operand::Value(99)),
		Instruction::Set('b', Operand::Ref('a')),
		Instruction::Jnz(Operand::Ref('g'), Operand::Value(5)),
		Instruction::Jnz(Operand::Value(1), Operand::Value(5)),
		Instruction::Mul('b', Operand::Value(100)),
		Instruction::Sub('b', Operand::Value(-100000))
	];
	assert_eq!(Instruction::parse(input), expected);
}


struct Processor {
	registers: HashMap<char, i32>,
	instructions: Vec<Instruction>,
	ip: i32,
	num_muls: u32
}

impl Processor {
	fn new() -> Processor {
		let mut registers = HashMap::new();

		for r in "abcdefgh".chars() {
			registers.insert(r, 0);
		}

		Processor {
			registers: registers,
			instructions: vec![],
			ip: 0,
			num_muls: 0
		}
	}

	fn execute(&mut self, instructions: Vec<Instruction>) {
		self.instructions = instructions;
		self.ip = 0;

		while self.ip >= 0 && (self.ip as usize) < self.instructions.len() {
			self.ip += self.execute_one(self.instructions[self.ip as usize])
				.unwrap_or(1);
		}
	}

	fn execute_one(&mut self, instruction: Instruction) -> Option<i32> {
		match instruction {
			Instruction::Set(reg, value) => {
				self.set(reg, self.evaluate(value));
				None
			}
			Instruction::Sub(reg, delta) => {
				self.set(reg, self.get(reg) - self.evaluate(delta));
				None
			}
			Instruction::Mul(reg, op) => {
				self.set(reg, self.get(reg) * self.evaluate(op));
				self.num_muls += 1;
				None
			}
			Instruction::Jnz(cond, offset) => {
				if self.evaluate(cond) == 0 {
					None
				} else {
					Some(self.evaluate(offset))
				}
			}
		}
	}
	
	fn evaluate(&self, operand: Operand) -> i32 {
		match operand {
			Operand::Value(v) => v,
			Operand::Ref(r) => self.get(r)
		}
	}

	fn get(&self, reg: char) -> i32 {
		self.registers.get(&reg)
			.unwrap_or_else(|| panic!("Can't get value for register '{}'", reg))
			.clone()
	}

	fn set(&mut self, reg: char, value: i32) {
		self.registers.insert(reg, value);
	}
}

#[test]
fn test_set() {
	let mut subject = Processor::new();
	subject.execute_one(Instruction::Set('c', Operand::Value(42)));
	assert_eq!(subject.get('c'), 42);
}

#[test]
fn test_sub() {
	let mut subject = Processor::new();
	subject.execute_one(Instruction::Sub('a', Operand::Value(17)));
	assert_eq!(subject.get('a'), -17);
	subject.execute_one(Instruction::Sub('a', Operand::Value(3)));
	assert_eq!(subject.get('a'), -20);
}

#[test]
fn test_mul() {
	let mut subject = Processor::new();
	subject.execute_one(Instruction::Set('a', Operand::Value(2)));
	subject.execute_one(Instruction::Mul('a', Operand::Value(-5)));
	assert_eq!(subject.get('a'), -10);
}

#[test]
fn test_executes_multiple() {
	let mut subject = Processor::new();
	subject.execute(vec![
		Instruction::Set('a', Operand::Value(1)),
		Instruction::Set('b', Operand::Value(2)),
	]);
	assert_eq!(subject.get('a'), 1);
	assert_eq!(subject.get('b'), 2);
}

#[test]
fn test_jnz_taken() {
	let mut subject = Processor::new();
	subject.execute(vec![
		Instruction::Set('a', Operand::Value(1)),
		Instruction::Jnz(Operand::Ref('a'), Operand::Value(2)),
		Instruction::Set('a', Operand::Value(-1)),
		Instruction::Set('b', Operand::Value(3)),
	]);
	assert_eq!(subject.get('a'), 1);
	assert_eq!(subject.get('b'), 3);
}

#[test]
fn test_jnz_taken_direct() {
	let mut subject = Processor::new();
	subject.execute(vec![
		Instruction::Jnz(Operand::Value(2), Operand::Value(2)),
		Instruction::Set('a', Operand::Value(-1)),
		Instruction::Set('b', Operand::Value(3)),
	]);
	assert_eq!(subject.get('a'), 0);
	assert_eq!(subject.get('b'), 3);
}

#[test]
fn test_jnz_not_taken() {
	let mut subject = Processor::new();
	subject.execute(vec![
		Instruction::Set('a', Operand::Value(0)),
		Instruction::Jnz(Operand::Ref('a'), Operand::Value(2)),
		Instruction::Set('a', Operand::Value(-1)),
		Instruction::Set('b', Operand::Value(3)),
	]);
	assert_eq!(subject.get('a'), -1);
	assert_eq!(subject.get('b'), 3);
}

#[test]
fn test_count_mul() {
	let mut subject = Processor::new();
	subject.execute(vec![
		Instruction::Mul('a', Operand::Value(0)),
		Instruction::Mul('a', Operand::Value(0)),
		Instruction::Set('a', Operand::Value(0)),
	]);
	assert_eq!(subject.num_muls, 2);
}
