#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
mod bot;
#[cfg(test)] mod testing_bot;
use machine::Machine;
use debugger::debug;

fn main() {
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
