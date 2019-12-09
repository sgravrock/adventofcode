#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::{Machine, MachineState};
use debugger::debug;
extern crate permutohedron;

fn main() {
	let program = input::puzzle_input();
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
