use crate::machine::Machine;
use crate::bot::{Bot, Cell, Direction};
use crate::debugger::debug;

#[derive(Clone)]
pub struct MachineBot {
	machine: Machine
}

impl MachineBot {
	pub fn new(input: Vec<i64>) -> MachineBot {
		MachineBot { machine: Machine::new(input) }
	}
}

impl Bot for MachineBot {
	fn advance(&mut self, dir: Direction) -> Cell {
		self.machine.input.enqueue(match dir {
			Direction::N => 1,
			Direction::S => 2,
			Direction::W => 3,
			Direction::E => 4,
		});

		execute_or_debug(&mut self.machine);
		let output = self.machine.output.dequeue().unwrap();

		match output {
			0 => Cell::Wall,
			1 => Cell::Empty,
			2 => Cell::Oxygen,
			_ => panic!("Unexpected output: {}", output)
		}
	}
}

fn execute_or_debug(mut machine: &mut Machine) {
	match machine.execute() {
		Ok(_) => {},
		Err(error) => {
			println!("Execution failed: {:?}", error);
			debug(&mut machine);
			panic!("Aborting because of previous execution failure");
		}
	}
}
