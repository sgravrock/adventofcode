use std::io;
use std::io::Write;
#[macro_use] extern crate text_io;
use std::cmp::min;
use std::convert::TryFrom;
extern crate permutohedron;

fn main() {
	let program = vec![3,8,1001,8,10,8,105,1,0,0,21,34,55,68,85,106,187,268,349,430,99999,3,9,1001,9,5,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,1001,9,2,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,101,3,9,9,102,3,9,9,4,9,99,3,9,1002,9,5,9,101,3,9,9,102,5,9,9,4,9,99,3,9,1002,9,4,9,1001,9,2,9,102,3,9,9,101,3,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99];
	println!("{}", find_max_thruster_signal(program).max_thruster_signal);
}

#[derive(Debug, PartialEq)]
struct SearchResult {
	max_thruster_signal: i32,
	phase_settings: Vec<i32>
}

fn find_max_thruster_signal(program: Vec<i32>) -> SearchResult {
	all_possible_phase_settings()
		.iter()
		.map(|phase_settings| {
			SearchResult {
				max_thruster_signal: thruster_signal(&program, &phase_settings),
				phase_settings: phase_settings.clone()
			}
		})
		.max_by_key(|sr| sr.max_thruster_signal)
		.unwrap()
}

#[test]
fn test_find_max_thruster_signal() {
	assert_eq!(
		find_max_thruster_signal(
			vec![3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
		),
		SearchResult {
			max_thruster_signal: 43210,
			phase_settings: vec![4, 3, 2, 1, 0]
		}
	);
	assert_eq!(
		find_max_thruster_signal(
			vec![3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,
				4,23,99,0,0]
		),
		SearchResult {
			max_thruster_signal: 54321,
			phase_settings: vec![0, 1, 2, 3, 4]
		}
	);
	assert_eq!(
		find_max_thruster_signal(
			vec![3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,
				33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
		),
		SearchResult {
			max_thruster_signal: 65210,
			phase_settings: vec![1, 0, 4, 3, 2]
		}
	);
}

fn all_possible_phase_settings() -> Vec<Vec<i32>> {
	let mut values = [0, 1, 2, 3, 4];
	let mut result: Vec<Vec<i32>> = vec![];
	permutohedron::heap_recursive(&mut values, |s| { result.push(s.to_vec()) });
	result
}

fn thruster_signal(program: &Vec<i32>, phase_settings: &Vec<i32>) -> i32 {
	phase_settings
		.iter()
		.fold(0, |input_signal, phase_setting| {
			let mut machine = Machine::new(program.clone());
			machine.input = vec![*phase_setting, input_signal];
			machine.execute().unwrap();
			assert_eq!(machine.output.len(), 1);
			machine.output[0]
		})
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Error {
	OutOfBoundsRead { index: i32 },
	OutOfBoundsWrite { index: i32 },
	InvalidOpcode { opcode: i32, ip: i32 },
	IpOutOfRange { ip: i32 },
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

struct ParamValues {
	arg0: Option<i32>,
	arg1: Option<i32>,
	dest: Option<i32>
}

struct Machine {
	mem: Vec<i32>,
	input: Vec<i32>,
	next_input_ix: usize,
	output: Vec<i32>,
	ip: i32,
}

impl Machine {
	fn new(mem: Vec<i32>) -> Machine {
		Machine { mem, input: vec![], next_input_ix: 0, output: vec![], ip: 0, }
	}

	fn execute(&mut self) -> Result<(), Error> {
		while self.read_instruction()? != 99 {
			self.do_current_instruction()?;
		}
		Ok(())
	}

	fn current_instruction(&self) -> Result<Instruction, Error> {
		self.read_instruction().and_then(|word| {
			Ok(Instruction::decode(word))
		})
	}

	fn read_instruction(&self) -> Result<i32, Error> {
		self.checked_read(self.ip as i32).or_else(|_| {
			Err(Error::IpOutOfRange {ip: self.ip as i32})
		})
	}

	fn do_current_instruction(&mut self) -> Result<(), Error> {
		let instruction = self.current_instruction()?;
		let params = self.evaluate_params(&instruction)?;
		let mut rvalue: Option<i32> = None;

		match instruction.opcode {
			1 => {
				rvalue = Some(params.arg0.unwrap() + params.arg1.unwrap());
				self.ip += 4;
			},
			2 => {
				rvalue = Some(params.arg0.unwrap() * params.arg1.unwrap());
				self.ip += 4;
			},
			3 => {
				// TODO: Propagate errors from being out of input
				rvalue = Some(self.input[self.next_input_ix]);
				self.next_input_ix += 1;
				self.ip += 2;
			},
			4 => {
				self.output.push(params.arg0.unwrap());
				self.ip += 2;
			},
			5 => {
				if params.arg0.unwrap() == 0 {
					self.ip += 3;
				} else {
					self.ip = params.arg1.unwrap();
				}
			},
			6 => {
				if params.arg0.unwrap() == 0 {
					self.ip = params.arg1.unwrap();
				} else {
					self.ip += 3;
				}
			},
			7 => {
				rvalue = Some(
					if params.arg0.unwrap() < params.arg1.unwrap() {
						1
					} else {
						0
					}
				);
				self.ip += 4;
			},
			8 => {
				rvalue = Some(
					if params.arg0.unwrap() == params.arg1.unwrap() {
						1
					} else {
						0
					}
				);
				self.ip += 4;
			},
			_ => {
				panic!("Unrecognized opcode {} at self.ip={} (should have been caught earlier", self.mem[self.ip as usize], self.ip)
			}
		}

		match rvalue {
			None => Ok(()),
			Some(v) => self.checked_write(params.dest.unwrap(), v)
		}
	}

	fn evaluate_params(&self, instruction: &Instruction)
			-> Result<ParamValues, Error> {
		match instruction.opcode {
			1 | 2 | 7 | 8 => Ok(ParamValues {
				arg0: Some(self.lvalue(&instruction, 0)?),
				arg1: Some(self.lvalue(&instruction, 1)?),
				dest: Some(self.checked_read(self.ip + 3)?),
			}),
			3 => Ok(ParamValues {
				arg0: None,
				arg1: None,
				dest: Some(self.checked_read(self.ip + 1)?),
			}),
			4 => Ok(ParamValues {
				arg0: Some(self.lvalue(&instruction, 0)?),
				arg1: None,
				dest: None
			}),
			5 | 6 => Ok(ParamValues {
				arg0: Some(self.lvalue(&instruction, 0)?),
				arg1: Some(self.lvalue(&instruction, 1)?),
				dest: None
			}),
			_ => Err(Error::InvalidOpcode {opcode: self.mem[self.ip as usize], ip: self.ip as i32})
		}
	}

	fn lvalue(&self, instruction: &Instruction, param_ix: usize)
			-> Result<i32, Error> {
		let x = self.checked_read(self.ip + param_ix as i32 + 1)?;
		match instruction.param_mode(param_ix) {
			Mode::Immed => Ok(x),
			Mode::Pos => self.checked_read(x)
		}
	}

	fn checked_read(&self, index: i32) -> Result<i32, Error> {
		match usize::try_from(index) {
			Err(_) => Err(Error::OutOfBoundsRead {index}),
			Ok(i) => match self.mem.get(i) {
				None => Err(Error::OutOfBoundsRead {index}),
				Some(v) => Ok(*v)
			}
		}
	}

	fn checked_write(&mut self, index: i32, value: i32) -> Result<(), Error> {
		match usize::try_from(index) {
			Err(_) => Err(Error::OutOfBoundsWrite {index}),
			Ok(i) => match self.mem.get_mut(i) {
				None => Err(Error::OutOfBoundsWrite {index}),
				Some(elem) => {
					*elem = value;
					Ok(())
				}
			}
		}
	}
}

#[test]
fn test_execute() {
	let mut machine = Machine::new(vec![1,9,10,3,2,3,11,0,99,30,40,50]);
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![3500,9,10,70,2,3,11,0,99,30,40,50]);
}

#[test]
fn test_execute_negative() {
	let mut machine = Machine::new(vec![1,5,6,7,99,-5,3,0]);
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![1,5,6,7,99,-5,3,-2]);
}

#[test]
fn test_execute_input() {
	let mut machine = Machine::new(vec![3,5,3,6,99,0,0]);
	machine.input = vec![10, 11];
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![3,5,3,6,99,10,11]);
}

#[test]
fn test_execute_output() {
	let mut machine = Machine::new(vec![4,3,99,12345]);
	machine.execute().unwrap();
	assert_eq!(machine.output, vec![12345]);
}

#[test]
fn test_execute_jump_if_true() {
	let mut machine1 = Machine::new(vec![1105,0,4,99,104,1,99]);
	machine1.execute().unwrap();
	assert_eq!(machine1.output, vec![]);
	let mut machine2 = Machine::new(vec![1105,1,4,99,4,1,99]);
	machine2.execute().unwrap();
	assert_eq!(machine2.output, vec![1]);
}

#[test]
fn test_execute_jump_if_false() {
	let mut machine1 = Machine::new(vec![1106,1,4,99,4,1,99]);
	machine1.execute().unwrap();
	assert_eq!(machine1.output, vec![]);

	let mut machine2 = Machine::new(vec![1106,0,4,99,104,1,99]);
	machine2.execute().unwrap();
	assert_eq!(machine2.output, vec![1]);
}

#[test]
fn test_less_than() {
	let mut machine1 = Machine::new(vec![1107,1,2,5,99,-1]);
	machine1.execute().unwrap();
	assert_eq!(machine1.mem, vec![1107,1,2,5,99,1]);

	let mut machine2 = Machine::new(vec![1107,2,2,5,99,-1]);
	machine2.execute().unwrap();
	assert_eq!(machine2.mem, vec![1107,2,2,5,99,0]);
}

#[test]
fn test_equal() {
	let mut machine1 = Machine::new(vec![1108,1,2,5,99,-1]);
	machine1.execute().unwrap();
	assert_eq!(machine1.mem, vec![1108,1,2,5,99,0]);

	let mut machine2 = Machine::new(vec![1108,2,2,5,99,-1]);
	machine2.execute().unwrap();
	assert_eq!(machine2.mem, vec![1108,2,2,5,99,1]);

	let mut machine3 = Machine::new(vec![1108,1,0,5,99,-1]);
	machine3.execute().unwrap();
	assert_eq!(machine3.mem, vec![1108,1,0,5,99,0]);
}

#[test]
fn test_execute_immediate_mode() {
	let mut machine = Machine::new(vec![1102,4,3,5,99,0]);
	machine.input = vec![5];
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![1102,4,3,5,99,12]);
}

#[test]
fn test_execute_detects_oob_reads() {
	let mut machine = Machine::new(vec![2, 500, 0, 0, 99]);
	assert_eq!(machine.execute(), Err(Error::OutOfBoundsRead {index: 500}));
	let mut machine2 = Machine::new(vec![2, 0, 500, 0, 99]);
	assert_eq!(machine2.execute(), Err(Error::OutOfBoundsRead {index: 500}));
	let mut machine3 = Machine::new(vec![2, 0, -1, 0, 99]);
	assert_eq!(machine3.execute(), Err(Error::OutOfBoundsRead {index: -1}));
}

#[test]
fn test_execute_detects_oob_writes() {
	let mut machine = Machine::new(vec![1102, 0, 0, 500, 99]);
	assert_eq!(machine.execute(), Err(Error::OutOfBoundsWrite {index: 500}));
	let mut machine2 = Machine::new(vec![1102, 0, 0, -1, 99]);
	assert_eq!(machine2.execute(), Err(Error::OutOfBoundsWrite {index: -1}));
}

#[test]
fn test_execute_detects_invalid_opcode() {
	let mut machine = Machine::new(vec![55]);
	assert_eq!(machine.execute(), Err(Error::InvalidOpcode {opcode: 55, ip: 0}));
}

#[test]
fn test_execute_detects_oob_jumps() {
	let mut machine = Machine::new(vec![1105,1,-1,99]);
	assert_eq!(machine.execute(), Err(Error::IpOutOfRange {ip: -1}));
}

fn debug(mut machine: &mut Machine) {
	println!("{} words of memory", machine.mem.len());
	show_machine_state(&machine);

	loop {
		print!("debugger> ");
		io::stdout().flush().unwrap();
		let line: String = read!("{}\n");
		let tokens: Vec<&str> = line.split(" ").collect();

		match tokens[0] {
			"q" | "quit" => return,
			"mem" => {
				dump_mem(tokens, &machine);
			},
			"s" | "step" => {
				debug_step(&mut machine);
			},
			"r" | "run" => {
				while machine.mem[machine.ip as usize] != 99 &&
						debug_step(&mut machine) {
				}
			},
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

fn debug_step(machine: &mut Machine) -> bool {
	let ok = match machine.do_current_instruction() {
		Ok(_) => true,
		Err(e) => {
			println!("{:#?}", e);
			false
		}
	};

	if machine.mem[machine.ip as usize] != 99 {
		show_machine_state(&machine);
	}

	ok
}

fn show_machine_state(machine: &Machine) {
	println!("ip={}", machine.ip);
	let ip = machine.ip as usize;
	println!("mem[ip..ip+3]={:?}", &machine.mem[ip..min(ip+4, machine.mem.len() - 1)]);
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
