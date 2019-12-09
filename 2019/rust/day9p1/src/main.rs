#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::{Machine, MachineState};
use debugger::debug;
extern crate permutohedron;

fn main() {
	let mut machine = Machine::new(input::puzzle_input());
	machine.input.enqueue(1);
	machine.execute().unwrap();
	println!("{}", machine.output.dequeue().unwrap());
	// 203 is not the correct answer
	assert!(machine.output.dequeue().is_none());
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
