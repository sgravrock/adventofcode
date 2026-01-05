#[macro_use] extern crate text_io;
mod machine;
mod debugger;
use std::fs;
use machine::Machine;
use debugger::debug;

fn main() {
	let mut machine = Machine::new(puzzle_input());
	machine.input.enqueue(2);
	execute_or_debug(&mut machine);
	let output = machine.output.contents().iter()
		.map(|n| char::from_u32(*n as u32).unwrap())
		.collect::<String>();
	println!("{}", output);
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

pub fn puzzle_input() -> Vec<i64> {
	let path = "../inputs/17";
	let contents = fs::read_to_string(path)
		.expect("Error reading input file");

	contents
		.trim()
		.split(",")
		.map(|chunk| {
			chunk.parse::<i64>()
				.unwrap_or_else(|_| panic!("Can't convert \"{}\" to a number", chunk))
				
		})
		.collect()
}

