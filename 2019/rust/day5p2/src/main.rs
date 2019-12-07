use std::io;
use std::io::Write;
#[macro_use] extern crate text_io;
use std::cmp::min;

fn main() {
	let input = 
		"3,225,1,225,6,6,1100,1,238,225,104,0,1102,78,40,225,1102,52,43,224,1001,224,-2236,224,4,224,102,8,223,223,101,4,224,224,1,224,223,223,1,191,61,224,1001,224,-131,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,86,74,225,1102,14,76,225,1101,73,83,224,101,-156,224,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,1102,43,82,225,2,196,13,224,101,-6162,224,224,4,224,102,8,223,223,101,5,224,224,1,223,224,223,1001,161,51,224,101,-70,224,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,102,52,187,224,1001,224,-832,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1102,19,79,225,101,65,92,224,1001,224,-147,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,1102,16,90,225,1102,45,44,225,1102,92,79,225,1002,65,34,224,101,-476,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,226,226,224,1002,223,2,223,1005,224,329,1001,223,1,223,1007,226,226,224,102,2,223,223,1005,224,344,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,359,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,389,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,404,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,419,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,449,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,464,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,479,101,1,223,223,107,226,677,224,102,2,223,223,1006,224,494,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,509,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,524,101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,539,101,1,223,223,1008,677,226,224,102,2,223,223,1005,224,554,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,569,101,1,223,223,1107,677,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1108,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,614,101,1,223,223,108,677,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1007,677,226,224,102,2,223,223,1006,224,644,101,1,223,223,8,677,677,224,1002,223,2,223,1006,224,659,101,1,223,223,8,677,226,224,102,2,223,223,1005,224,674,101,1,223,223,4,223,99,226";
	let mut machine = Machine::new(parse_program(input));
	machine.input = Some(5);
	debug(&mut machine);
	println!("Output: {:?}", machine.output);
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

struct Machine {
	mem: Vec<i32>,
	input: Option<i32>,
	output: Vec<i32>,
	ip: usize,
}

impl Machine {
	fn new(mem: Vec<i32>) -> Machine {
		Machine { mem, input: None, output: vec![], ip: 0, }
	}

	fn execute(&mut self) {
		while self.mem[self.ip] != 99 {
			self.do_current_instruction();
		}
	}

	fn current_instruction(&self) -> Instruction {
		Instruction::decode(self.mem[self.ip])
	}

	fn do_current_instruction(&mut self) {
		let instruction = self.current_instruction();

		if instruction.opcode == 1  || instruction.opcode == 2 {
			let arg0 = self.lvalue(&instruction, 0);
			let arg1 = self.lvalue(&instruction, 1);
			let dest = self.mem[self.ip + 3] as usize;
	
			if instruction.opcode == 1 {
				self.mem[dest] = arg0 + arg1;
			} else {
				self.mem[dest] = arg0 * arg1;
			}
	
			self.ip += 4;
		} else if instruction.opcode == 3 {
			let dest = self.mem[self.ip + 1] as usize;
			self.mem[dest] = self.input.unwrap();
			self.ip += 2;
		} else if instruction.opcode == 4 {
			let v = self.lvalue(&instruction, 0);
			self.output.push(v);
			self.ip += 2;
		} else if instruction.opcode == 5 {
			let cond = self.lvalue(&instruction, 0);
			if cond == 0 {
				self.ip += 3;
			} else {
				self.ip = self.lvalue(&instruction, 1) as usize;
			}
		} else if instruction.opcode == 6 {
			let cond = self.lvalue(&instruction, 0);
			if cond == 0 {
				self.ip = self.lvalue(&instruction, 1) as usize;
			} else {
				self.ip += 3;
			}
		} else if instruction.opcode == 7 {
			let a = self.lvalue(&instruction, 0);
			let b = self.lvalue(&instruction, 1);
			let dest = self.mem[self.ip + 3] as usize;
			self.mem[dest] = if a < b { 1 } else { 0 };
			self.ip = 4;
		} else if instruction.opcode == 8 {
			let a = self.lvalue(&instruction, 0);
			let b = self.lvalue(&instruction, 1);
			let dest = self.mem[self.ip + 3] as usize;
			self.mem[dest] = if a == b { 1 } else { 0 };
			self.ip = 4;
		} else {
			panic!("Unrecognized opcode {} at self.ip={}", self.mem[self.ip], self.ip);
		}
	}

	fn lvalue(&self, instruction: &Instruction, param_ix: usize) -> i32 {
		let x = self.mem[self.ip + param_ix + 1];
		match instruction.param_mode(param_ix) {
			Mode::Immed => x,
			Mode::Pos => self.mem[x as usize]
		}
	}
}

#[test]
fn test_execute() {
	let mut machine = Machine::new(vec![1,9,10,3,2,3,11,0,99,30,40,50]);
	machine.execute();
	assert_eq!(machine.mem, vec![3500,9,10,70,2,3,11,0,99,30,40,50]);
}

#[test]
fn test_execute_negative() {
	let mut machine = Machine::new(vec![1,5,6,7,99,-5,3,0]);
	machine.execute();
	assert_eq!(machine.mem, vec![1,5,6,7,99,-5,3,-2]);
}

#[test]
fn test_execute_input() {
	let mut machine = Machine::new(vec![3,4,99,0,0]);
	machine.input = Some(5);
	machine.execute();
	assert_eq!(machine.mem, vec![3,4,99,0,5]);
}

#[test]
fn test_execute_output() {
	let mut machine = Machine::new(vec![4,3,99,12345]);
	machine.execute();
	assert_eq!(machine.output, vec![12345]);
}

#[test]
fn test_execute_jump_if_true() {
	let mut machine1 = Machine::new(vec![1105,0,4,99,104,1,99]);
	machine1.execute();
	assert_eq!(machine1.output, vec![]);
	let mut machine2 = Machine::new(vec![1105,1,4,99,4,1,99]);
	machine2.execute();
	assert_eq!(machine2.output, vec![1]);
}

#[test]
fn test_execute_jump_if_false() {
	let mut machine1 = Machine::new(vec![1106,1,4,99,4,1,99]);
	machine1.execute();
	assert_eq!(machine1.output, vec![]);

	let mut machine2 = Machine::new(vec![1106,0,4,99,104,1,99]);
	machine2.execute();
	assert_eq!(machine2.output, vec![1]);
}

#[test]
fn test_less_than() {
	let mut machine1 = Machine::new(vec![1107,1,2,5,99,-1]);
	machine1.execute();
	assert_eq!(machine1.mem, vec![1107,1,2,5,99,1]);

	let mut machine2 = Machine::new(vec![1107,2,2,5,99,-1]);
	machine2.execute();
	assert_eq!(machine2.mem, vec![1107,2,2,5,99,0]);
}

#[test]
fn test_equal() {
	let mut machine1 = Machine::new(vec![1108,1,2,5,99,-1]);
	machine1.execute();
	assert_eq!(machine1.mem, vec![1108,1,2,5,99,0]);

	let mut machine2 = Machine::new(vec![1108,2,2,5,99,-1]);
	machine2.execute();
	assert_eq!(machine2.mem, vec![1108,2,2,5,99,1]);

	let mut machine3 = Machine::new(vec![1108,1,0,5,99,-1]);
	machine3.execute();
	assert_eq!(machine3.mem, vec![1108,1,0,5,99,0]);
}

#[test]
fn test_execute_immediate_mode() {
	let mut machine = Machine::new(vec![1102,4,3,5,99,0]);
	machine.input = Some(5);
	machine.execute();
	assert_eq!(machine.mem, vec![1102,4,3,5,99,12]);
}

fn debug(machine: &mut Machine) {
	println!("{} words of memory", machine.mem.len());
	show_machine_state(&machine);

	loop {
		print!("debugger> ");
		io::stdout().flush();
		let line: String = read!("{}\n");
		let tokens: Vec<&str> = line.split(" ").collect();

		match tokens[0] {
			"q" | "quit" => return,
			"mem" => {
				dump_mem(tokens, &machine);
			},
			"s" | "step" => {
				machine.do_current_instruction();
				show_machine_state(&machine);
			},
			"r" | "run" => {
				while machine.mem[machine.ip] != 99 {
					machine.do_current_instruction();
					if machine.mem[machine.ip] != 99 {
						show_machine_state(&machine);
					}
				}
			}
			_ => {
				println!("Commands:");
				println!("mem [start end]: dump memory");
				println!("s, step:         perform the instruction at ip");
				println!("r, run:          run from current position");
				println!("q, quit:         exit");
			}
		}
	}
}

fn show_machine_state(machine: &Machine) {
	println!("ip={}", machine.ip);
	println!("mem[ip..ip+3]={:?}", &machine.mem[machine.ip..min(machine.ip+4, machine.mem.len() - 1)]);
	println!("instruction at ip: {:?}", machine.current_instruction());
}

fn dump_mem(cmd: Vec<&str>, machine: &Machine) {
	match cmd.len() {
		1 => println!("{:?}", machine.mem),
		3 => {
			match cmd[1].parse::<usize>() {
				Ok(start) => {
					match cmd[2].parse::<usize>() {
						Ok(end) => {
							if start < end {
								println!("{:?}", &machine.mem[start..end+1]);
							} else {
								println!("Start must be before end");
							}
						},
						Err(_) => println!("End must be a nonnegative int")
					}
				},
				Err(_) => println!("Start must be a nonnegative int")
			}
		},
		_ => println!("Usage: mem [start end]")
	};
}
