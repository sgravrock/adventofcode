#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::Machine;
use debugger::debug;
extern crate permutohedron;

fn main() {
	let mut machine = Machine::new(input::puzzle_input());
	machine.input.enqueue(1);
	execute_or_debug(&mut machine);
	let output = machine.output.contents();

	match output.len() {
		1 => println!("Success. Output is {}.", output[0]),
		_ => println!("Failure. Output is {:?}.", output),
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
