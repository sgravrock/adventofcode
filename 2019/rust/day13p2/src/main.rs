#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::Machine;
use debugger::debug;
use std::collections::HashMap;
use std::io::{stdin, stdout};
use std::io::Read;
use std::io::Write;

fn main() {
	Game::new().play();
}

struct Game {
	machine: Machine,
	screen: HashMap<(i64, i64), char>,
	score: i64,
}

impl Game {
	fn new() -> Game {
		let mut mem = input::puzzle_input();
		mem[0] = 2;
		let machine = Machine::new(mem);
		Game { machine, screen: HashMap::new(), score: 0 }
	}

	fn play(&mut self) {
		self.machine.input.enqueue(2);

		loop {
			execute_or_debug(&mut self.machine);
			self.show();
			let cmd = self.prompt();
			self.machine.input.enqueue(cmd);
		}
	}

	fn prompt(&mut self) -> i64 {
		loop {
			print!("Enter joystick command (-1=left, 0=middle, 1=right): ");
			stdout().lock().flush().unwrap();
			let mut line = String::new();
			stdin().read_line(&mut line).unwrap();

			match line.as_str() {
				"-1\n" => return -1,
				"0\n" => return 0,
				"1\n" => return 1,
				_ => {}
			}
		}
	}

	fn show(&mut self) {
		for chunk in self.machine.output.dequeue_all().chunks(3) {
			if chunk[0] == -1 && chunk[1] == 0 {
				self.score = chunk[2];
			} else {
				self.screen.insert((chunk[0], chunk[1]), match chunk[2] {
					0 => ' ',
					1 => '=',
					2 => '-',
					3 => '_',
					4 => '*',
					_ => panic!("Unrecognized tile {}", chunk[2])
				});
			}
		}
	
		let xmax = self.screen.keys().map(|(x, _)| *x).max().unwrap();
		let ymax = self.screen.keys().map(|(_, y)| *y).max().unwrap();
	
		for y in 0..ymax {
			for x in 0..xmax {
				print!("{}", self.screen.get(&(x, y)).unwrap_or(&' '));
			}
			println!("");
		}
	
		println!("Score: {}", self.score);
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
