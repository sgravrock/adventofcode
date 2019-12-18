#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::{Machine, MachineState};
use debugger::debug;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
	let grid = paint_panels(input::puzzle_input());
	let xmin = grid.keys().map(|(x, _)| *x).min().unwrap();
	let xmax = grid.keys().map(|(x, _)| *x).max().unwrap() + 1;
	let ymin = grid.keys().map(|(_, y)| *y).min().unwrap();
	let ymax = grid.keys().map(|(_, y)| *y).max().unwrap() + 1;

	for y in ymin..ymax {
		for x in xmin..xmax {
			print!("{}", match grid.get(&(x, y)) {
				Some(1) => '#',
				_ => '.'
			});
		}

		println!("");
	}
}

type Coord = (i64, i64);

fn paint_panels(program: Vec<i64>) -> HashMap<Coord, i64> {
	let directions = vec![
		(0, -1),
		(1, 0),
		(0, 1),
		(-1, 0)
	];
	let mut grid: HashMap<Coord, i64> = HashMap::new();
	let mut painted: HashSet<Coord> = HashSet::new();
	let mut robot_pos = (0, 0);
	let mut robot_direction: isize = 0;
	let mut machine = Machine::new(program);

	grid.insert(robot_pos, 1);

	while machine.state != MachineState::Halted {
		let color = grid.get(&robot_pos).unwrap_or(&0);
		machine.input.enqueue(*color);

		execute_or_debug(&mut machine);

		if machine.state != MachineState::Halted {
			// Paint the current square
			grid.insert(robot_pos, machine.output.dequeue().unwrap());
			painted.insert(robot_pos);
	
			// Move the robot
			robot_direction =  match machine.output.dequeue().unwrap() {
				0 => if robot_direction == 0 { 3 } else { robot_direction - 1 },
				1 => (robot_direction + 1) % 4,
				x => panic!("Unexpected turn: {}", x)
			};
			robot_pos = (
				robot_pos.0 + directions[robot_direction as usize].0,
				robot_pos.1 + directions[robot_direction as usize].1
			);
		}
	}

	grid
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
