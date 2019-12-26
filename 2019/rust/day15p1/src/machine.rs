use std::convert::TryFrom;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Error {
	OutOfBoundsRead { index: i64 },
	OutOfBoundsWrite { index: i64 },
	InvalidOpcode { opcode: i64, ip: i64 },
	IpOutOfRange { ip: i64 },
	ImmediateModeDestParam,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Mode {
	Immed,
	Pos,
	Rel,
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
	opcode: i64,
	param_modes: Vec<Mode>
}

impl Instruction {
	fn decode(mut word: i64) -> Instruction {
		let opcode = word % 100;
		word /= 100;
		let mut param_modes: Vec<Mode> = vec![];

		while word > 0 {
			param_modes.push(match word % 10 {
				0 => Mode::Pos,
				1 => Mode::Immed,
				2 => Mode::Rel,
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

struct ParamValues {
	arg0: Option<i64>,
	arg1: Option<i64>,
	dest: Option<i64>
}

#[derive(Debug, PartialEq)]
pub enum MachineState {
	Running,
	Blocked,
	Halted
}

pub enum Event {
	MemWrite(usize, i64),
	Output(i64),
	Input(Option<i64>),
	RelBaseSet(i64),
}

pub type EventHandler = fn(event: Event);

pub struct Machine {
	pub mem: Vec<i64>,
	pub input: Queue<i64>,
	pub output: Queue<i64>,
	pub ip: i64,
	pub relative_base: i64,
	pub state: MachineState,
	pub event_handler: Option<EventHandler>,
}

impl Machine {
	pub fn new(mem: Vec<i64>) -> Machine {
		Machine {
			mem,
			input: Queue::new(),
			output: Queue::new(),
			ip: 0,
			relative_base: 0,
			state: MachineState::Running,
			event_handler: None,
		}
	}

	pub fn execute(&mut self) -> Result<(), Error> {
		self.state = MachineState::Running;
		while self.state == MachineState::Running {
			self.do_current_instruction()?;
		}
		Ok(())
	}

	pub fn current_instruction(&self) -> Result<Instruction, Error> {
		self.read_instruction().and_then(|word| {
			Ok(Instruction::decode(word))
		})
	}

	fn read_instruction(&self) -> Result<i64, Error> {
		self.checked_read(self.ip as i64).or_else(|_| {
			Err(Error::IpOutOfRange {ip: self.ip as i64})
		})
	}

	pub fn do_current_instruction(&mut self) -> Result<(), Error> {
		let instruction = self.current_instruction()?;
		let params = self.evaluate_params(&instruction)?;
		let mut to_store: Option<i64> = None;

		match instruction.opcode {
			1 => {
				to_store = Some(params.arg0.unwrap() + params.arg1.unwrap());
				self.ip += 4;
			},
			2 => {
				to_store = Some(params.arg0.unwrap() * params.arg1.unwrap());
				self.ip += 4;
			},
			3 => {
				match self.input.dequeue() {
					Some(input) => {
						to_store = Some(input);
						self.ip += 2;
					},
					None => self.state = MachineState::Blocked
				}
				self.emit_event(Event::Input(to_store));
			},
			4 => {
				let value = params.arg0.unwrap();
				self.output.enqueue(value);
				self.emit_event(Event::Output(value));
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
				to_store = Some(
					if params.arg0.unwrap() < params.arg1.unwrap() {
						1
					} else {
						0
					}
				);
				self.ip += 4;
			},
			8 => {
				to_store = Some(
					if params.arg0.unwrap() == params.arg1.unwrap() {
						1
					} else {
						0
					}
				);
				self.ip += 4;
			},
			9 => {
				let offset = params.arg0.unwrap();
				self.relative_base += offset;
				self.emit_event(Event::RelBaseSet(offset));
				self.ip += 2;
			}
			99 => {
				self.state = MachineState::Halted;
			}
			_ => {
				panic!("Unrecognized opcode {} at self.ip={} (should have been caught earlier", self.mem[self.ip as usize], self.ip)
			}
		}

		match to_store {
			None => Ok(()),
			Some(v) => self.checked_write(params.dest.unwrap(), v)
		}
	}

	fn evaluate_params(&self, instruction: &Instruction)
			-> Result<ParamValues, Error> {
		match instruction.opcode {
			1 | 2 | 7 | 8 => Ok(ParamValues {
				arg0: Some(self.rvalue(&instruction, 0)?),
				arg1: Some(self.rvalue(&instruction, 1)?),
				dest: Some(self.lvalue(&instruction, 2)?),
			}),
			3 => Ok(ParamValues {
				arg0: None,
				arg1: None,
				dest: Some(self.lvalue(&instruction, 0)?),
			}),
			4 => Ok(ParamValues {
				arg0: Some(self.rvalue(&instruction, 0)?),
				arg1: None,
				dest: None
			}),
			5 | 6 => Ok(ParamValues {
				arg0: Some(self.rvalue(&instruction, 0)?),
				arg1: Some(self.rvalue(&instruction, 1)?),
				dest: None
			}),
			9 => Ok(ParamValues {
				arg0: Some(self.rvalue(&instruction, 0)?),
				arg1: None,
				dest: None,
			}),
			99 => Ok(ParamValues {
				arg0: None,
				arg1: None,
				dest: None,
			}),
			_ => Err(Error::InvalidOpcode {opcode: self.mem[self.ip as usize], ip: self.ip as i64})
		}
	}

	fn lvalue(&self, instruction: &Instruction, param_ix: usize)
			-> Result<i64, Error> {
		let x = self.checked_read(self.ip + param_ix as i64 + 1)?;
		match instruction.param_mode(param_ix) {
			Mode::Immed => Err(Error::ImmediateModeDestParam),
			Mode::Pos => Ok(x),
			Mode::Rel => Ok(x + self.relative_base),
		}
	}

	fn rvalue(&self, instruction: &Instruction, param_ix: usize)
			-> Result<i64, Error> {
		let x = self.checked_read(self.ip + param_ix as i64 + 1)?;
		match instruction.param_mode(param_ix) {
			Mode::Immed => Ok(x),
			Mode::Pos => self.checked_read(x),
			Mode::Rel => self.checked_read(x + self.relative_base),
		}
	}

	fn checked_read(&self, index: i64) -> Result<i64, Error> {
		match usize::try_from(index) {
			Err(_) => Err(Error::OutOfBoundsRead {index}),
			Ok(i) => match self.mem.get(i) {
				None => Ok(0),
				Some(v) => Ok(*v)
			}
		}
	}

	fn checked_write(&mut self, index: i64, value: i64) -> Result<(), Error> {
		match usize::try_from(index) {
			Err(_) => Err(Error::OutOfBoundsWrite {index}),
			Ok(i) => {
				if i >= self.mem.len() {
					self.mem.resize(i + 1, 0);
				}

				self.mem[i] = value;
				self.emit_event(Event::MemWrite(i, value));
				Ok(())
			}
		}
	}

	fn emit_event(&self, event: Event) {
		match self.event_handler {
			Some(handler) => handler(event),
			None => {}
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
	machine.input.enqueue(10);
	machine.input.enqueue(11);
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![3,5,3,6,99,10,11]);
}

#[test]
fn test_execute_input_pauses() {
	let mut machine = Machine::new(vec![3,3,99,0]);
	machine.execute().unwrap();
	assert_eq!(machine.state, MachineState::Blocked);
	machine.input.enqueue(10);
	machine.execute().unwrap();
	assert_eq!(machine.state, MachineState::Halted);
	assert_eq!(machine.mem, vec![3,3,99,10]);
}

#[test]
fn test_execute_output() {
	let mut machine = Machine::new(vec![4,3,99,12345]);
	machine.execute().unwrap();
	assert_eq!(machine.output.dequeue(), Some(12345));
}

#[test]
fn test_execute_jump_if_true() {
	let mut machine1 = Machine::new(vec![1105,0,4,99,104,1,99]);
	machine1.execute().unwrap();
	assert_eq!(machine1.output.dequeue(), None);
	let mut machine2 = Machine::new(vec![1105,1,4,99,4,1,99]);
	machine2.execute().unwrap();
	assert_eq!(machine2.output.dequeue(), Some(1));
}

#[test]
fn test_execute_jump_if_false() {
	let mut machine1 = Machine::new(vec![1106,1,4,99,4,1,99]);
	machine1.execute().unwrap();
	assert_eq!(machine1.output.dequeue(), None);

	let mut machine2 = Machine::new(vec![1106,0,4,99,104,1,99]);
	machine2.execute().unwrap();
	assert_eq!(machine2.output.dequeue(), Some(1));
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
	machine.input.enqueue(5);
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![1102,4,3,5,99,12]);
}

#[test]
fn test_execute_relative_mode() {
	let program = vec![204,9,109,19,109,-2,204,-7,99,50,51];
	let mut machine = Machine::new(program);
	machine.execute().unwrap();
	assert_eq!(machine.output.contents(), vec![50, 51]);
}

#[test]
fn test_execute_allows_positive_oob_reads() {
	let mut machine = Machine::new(vec![4,3,99]);
	machine.execute().unwrap();
	assert_eq!(machine.output.dequeue(), Some(0));
}

#[test]
fn test_execute_detects_negative_oob_reads() {
	let mut machine = Machine::new(vec![2, 0, -1, 0, 99]);
	assert_eq!(machine.execute(), Err(Error::OutOfBoundsRead {index: -1}));
}

#[test]
fn test_execute_allows_positive_oob_writes() {
	let mut machine = Machine::new(vec![1101, 1, 2, 6, 99]);
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![1101, 1, 2, 6, 99, 0, 3]);
}

#[test]
fn test_execute_detects_negative_oob_writes() {
	let mut machine = Machine::new(vec![1102, 0, 0, -1, 99]);
	assert_eq!(machine.execute(), Err(Error::OutOfBoundsWrite {index: -1}));
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

#[test]
fn test_combined_day9_1_features_1() {
	let program = vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];
	let mut machine = Machine::new(program.clone());
	machine.execute().unwrap();
	assert_eq!(machine.output.contents(), program);
}

#[test]
fn test_combined_day9_1_features_2() {
	let program = vec![1102,34915192,34915192,7,4,7,99,0];
	let mut machine = Machine::new(program.clone());
	machine.execute().unwrap();
	let output = machine.output.dequeue().unwrap();
	assert_eq!(num_to_digits(output).len(), 16);
}

#[test]
fn test_combined_day9_1_features_3() {
	let program = vec![104,1125899906842624,99];
	let mut machine = Machine::new(program.clone());
	machine.execute().unwrap();
	assert_eq!(machine.output.contents(), vec![1125899906842624]);
}

#[test]
fn test_execute_203() {
	let mut machine = Machine::new(vec![109,10,203,-5,99,-1]);
	machine.input.enqueue(50);
	machine.execute().unwrap();
	assert_eq!(machine.mem, vec![109,10,203,-5,99,50]);
}

#[cfg(test)]
fn num_to_digits(mut n: i64) -> Vec<i64> {
	let mut digits: Vec<i64> = vec![];

	while n > 0 {
		digits.insert(0, n % 10);
		n /= 10;
	}

	digits
}


pub struct Queue<T> where T: Copy {
	buf: Vec<T>
}

impl<T> Queue<T> where T: Copy {
	pub fn new() -> Queue<T> {
		Queue {buf: vec![]}
	}

	pub fn enqueue(&mut self, item: T) {
		self.buf.insert(0, item);
	}

	pub fn dequeue(&mut self) -> Option<T> {
		self.buf.pop()
	}

	pub fn dequeue_all(&mut self) -> Vec<T> {
		let result = self.contents();
		self.buf.truncate(0);
		result
	}

	pub fn contents(&self) -> Vec<T> {
		reverse(&self.buf)
	}
}

fn reverse<T>(input: &Vec<T>) -> Vec<T> where T: Copy {
	input.iter().rev().map(|&x| x).collect()
}
