#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::Machine;
use debugger::debug;
use std::collections::HashMap;

fn main() {
	let mut machine = Machine::new(input::puzzle_input());
	machine.input.enqueue(2);
	execute_or_debug(&mut machine);
	let mut screen: HashMap<(i64, i64), char> = HashMap::new();

	for chunk in machine.output.contents().chunks(3) {
		screen.insert((chunk[0], chunk[1]), match chunk[2] {
			0 => ' ',
			1 => '=',
			2 => '-',
			3 => '_',
			4 => '*',
			_ => panic!("Unrecognized tile {}", chunk[2])
		});
	}

	let xmax = screen.keys().map(|(x, _)| *x).max().unwrap();
	let ymax = screen.keys().map(|(_, y)| *y).max().unwrap();

	for y in 0..ymax {
		for x in 0..xmax {
			print!("{}", screen.get(&(x, y)).unwrap_or(&' '));
		}
		println!("");
	}

	let nblocks = screen.values().filter(|tile| **tile == '-').count();
	println!("{} blocks.", nblocks);
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
