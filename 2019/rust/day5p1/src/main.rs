fn main() {
	unimplemented!();
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Inputs {
	noun: i32,
	verb: i32,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Mode {
	Immed,
	Pos
}

#[derive(Debug, PartialEq)]
struct Instruction {
	opcode: i32,
	param_modes: Vec<Mode>
}

impl Instruction {
	fn decode(mut word: i32) -> Instruction {
		let opcode = word % 100;
		word /= 100;
		let mut param_modes: Vec<Mode> = vec![];

		while word > 0 {
			param_modes.insert(0, match word % 10 {
				0 => Mode::Pos,
				1 => Mode::Immed,
				_ => panic!("Unrecognized parameter mode {}", word % 10)
			});
			word /= 10;
		}

		Instruction {opcode, param_modes}
	}

	fn param_mode(&self, i: usize) -> Mode {
		match self.param_modes.get(i) {
			Some(x) => *x,
			None => Mode::Pos
		}
	}
}

#[test]
fn test_instruction_decode() {
	assert_eq!(
		Instruction::decode(1002),
		Instruction {opcode: 2, param_modes: vec![Mode::Immed, Mode::Pos]}
	);
	assert_eq!(
		Instruction::decode(2),
		Instruction {opcode: 2, param_modes: vec![]}
	);
}

fn parse_program(input: &str) -> Vec<i32> {
	input
		.split(",")
		.map({|x| x.parse::<i32>().unwrap()})
		.collect()
}

#[test]
fn test_parse_program() {
	assert_eq!(parse_program("5,0,99,2,14"), vec![5,0,99,2,14]);
}

fn execute(program: &mut Vec<i32>, input: Option<i32>) -> Vec<i32> {
	let mut ip = 0;
	let mut output: Vec<i32> = vec![];

	while program[ip] != 99 {
		let instruction = Instruction::decode(program[ip]);

		if instruction.opcode == 1  || instruction.opcode == 2 {
			let arg0 = lvalue(program, &instruction, ip, 0);
			let arg1 = lvalue(program, &instruction, ip, 1);
			let dest = program[ip + 3] as usize;
	
			if instruction.opcode == 1 {
				program[dest] = arg0 + arg1;
			} else {
				program[dest] = arg0 * arg1;
			}
	
			ip += 4;
		} else if instruction.opcode == 3 {
			let dest = program[ip + 1] as usize;
			program[dest] = input.unwrap();
			ip += 2;
		} else if instruction.opcode == 4 {
			let v = lvalue(program, &instruction, ip, 0);
			output.push(v);
			ip += 2;
		} else {
			panic!("Unrecognized opcode {} at ip={}", program[ip], ip);
		}

	}

	output
}

fn lvalue(program: &Vec<i32>, instruction: &Instruction, ip: usize, param_ix: usize) -> i32 {
	let x = program[ip + param_ix + 1];
	match instruction.param_mode(param_ix as usize) {
		Mode::Immed => x,
		Mode::Pos => program[x as usize]
	}
}

#[test]
fn test_execute() {
	let mut program = vec![1,9,10,3,2,3,11,0,99,30,40,50];
	let expected = vec![3500,9,10,70,2,3,11,0,99,30,40,50];
	execute(&mut program, None);
	assert_eq!(program, expected);
}

#[test]
fn test_execute_negative() {
	let mut program = vec![1,5,6,7,99,-5,3,0];
	let expected = vec![1,5,6,7,99,-5,3,-2];
	execute(&mut program, None);
	assert_eq!(program, expected);
}

#[test]
fn test_execute_input() {
	let mut program = vec![3,4,99,0,0];
	let expected = vec![3,4,99,0,5];
	execute(&mut program, Some(5));
	assert_eq!(program, expected);
}

#[test]
fn test_execute_output() {
	let mut program = vec![4,3,99,12345];
	assert_eq!(execute(&mut program, None), vec![12345]);
}

#[test]
fn test_execute_immediate_mode() {
	let mut program = vec![1102,4,3,5,99,0];
	let expected = vec![1102,4,3,5,99,12];
	execute(&mut program, Some(5));
	assert_eq!(program, expected);
}
