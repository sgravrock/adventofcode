#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::Machine;
use debugger::debug;
extern crate permutohedron;

fn main() {
	let mut machine = Machine::new(input::puzzle_input());
	machine.input.enqueue(2);
	execute_or_debug(&mut machine);
	let output = machine.output.contents();
	let n = output.iter()
		.filter(|n| **n == 2)
		.count();
	println!("{:?}", n); // 233 is too high
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
