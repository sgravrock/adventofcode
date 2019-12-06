fn main() {
	let input = 
		"3,225,1,225,6,6,1100,1,238,225,104,0,1102,78,40,225,1102,52,43,224,1001,224,-2236,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1,191,61,224,1001,224,-131,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,86,74,225,1102,14,76,225,1101,73,83,224,101,-156,224,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,43,82,225,2,196,13,224,101,-6162,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,161,51,224,101,-70,224,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,102,52,187,224,1001,224,-832,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1102,19,79,225,101,65,92,224,1001,224,-147,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,16,90,225,1102,45,44,225,1102,92,79,225,1002,65,34,224,101,-476,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,344,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,404,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,509,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,539,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,569,101,1,223,223,1107,677,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,614,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,644,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,659,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226";
	let mut program = parse_program(input);
	let output = execute(&mut program, Some(5));
	println!("Output: {:?}", output);
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
			param_modes.push(match word % 10 {
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
		Instruction {opcode: 2, param_modes: vec![Mode::Pos, Mode::Immed]}
	);
	assert_eq!(
		Instruction::decode(2),
		Instruction {opcode: 2, param_modes: vec![]}
	);
	assert_eq!(
		Instruction::decode(103),
		Instruction {opcode: 3, param_modes: vec![Mode::Immed]}
	);
	assert_eq!(
		Instruction::decode(104),
		Instruction {opcode: 4, param_modes: vec![Mode::Immed]}
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
		} else if instruction.opcode == 5 {
			let cond = lvalue(program, &instruction, ip, 0);
			if cond == 0 {
				ip += 3;
			} else {
				ip = lvalue(program, &instruction, ip, 1) as usize;
			}
		} else if instruction.opcode == 6 {
			let cond = lvalue(program, &instruction, ip, 0);
			if cond == 0 {
				ip = lvalue(program, &instruction, ip, 1) as usize;
			} else {
				ip += 3;
			}
		} else if instruction.opcode == 7 {
			let a = lvalue(program, &instruction, ip, 0);
			let b = lvalue(program, &instruction, ip, 1);
			let dest = program[ip + 3] as usize;
			program[dest] = if a < b { 1 } else { 0 };
			ip = 4;
		} else if instruction.opcode == 8 {
			let a = lvalue(program, &instruction, ip, 0);
			let b = lvalue(program, &instruction, ip, 1);
			let dest = program[ip + 3] as usize;
			program[dest] = if a == b { 1 } else { 0 };
			ip = 4;
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
fn test_execute_jump_if_true() {
	let mut program1 = vec![1105,0,4,99,104,1,99];
	assert_eq!(execute(&mut program1, None), vec![]);
	let mut program2 = vec![1105,1,4,99,4,1,99];
	assert_eq!(execute(&mut program2, None), vec![1]);
}

#[test]
fn test_execute_jump_if_false() {
	let mut program1 = vec![1106,1,4,99,4,1,99];
	assert_eq!(execute(&mut program1, None), vec![]);
	let mut program2 = vec![1106,0,4,99,104,1,99];
	assert_eq!(execute(&mut program2, None), vec![1]);
}

#[test]
fn test_less_than() {
	let mut program1 = vec![1107,1,2,5,99,-1];
	execute(&mut program1, None);
	assert_eq!(program1, vec![1107,1,2,5,99,1]);
	let mut program2 = vec![1107,2,2,5,99,-1];
	execute(&mut program2, None);
	assert_eq!(program2, vec![1107,2,2,5,99,0]);
}

#[test]
fn test_equal() {
	let mut program1 = vec![1108,1,2,5,99,-1];
	execute(&mut program1, None);
	assert_eq!(program1, vec![1108,1,2,5,99,0]);

	let mut program2 = vec![1108,2,2,5,99,-1];
	execute(&mut program2, None);
	assert_eq!(program2, vec![1108,2,2,5,99,1]);

	let mut program3 = vec![1108,1,0,5,99,-1];
	execute(&mut program3, None);
	assert_eq!(program3, vec![1108,1,0,5,99,0]);
}

#[test]
fn test_execute_immediate_mode() {
	let mut program = vec![1102,4,3,5,99,0];
	let expected = vec![1102,4,3,5,99,12];
	execute(&mut program, Some(5));
	assert_eq!(program, expected);
}
