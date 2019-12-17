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
		let mut prev_ball_pos: Option<(i64, i64)> = None;

		loop {
			execute_or_debug(&mut self.machine);
			self.show();

			if self.num_blocks() == 0 {
				println!("You have won. Final score: {}", self.score);
				return;
			}

			let bp = self.ball_pos();
			if bp.1 == self.ymax() {
				println!("You have lost.");
				return;
			}

			let pp = self.paddle_pos();
			let cmd = if bp.0 < pp.0 {
				-1
			} else if bp.0 > pp.0 {
				1
			} else {
				println!("bp=pp");
				if bp.1 == pp.1 - 1 {
					// Don't move out from under the ball.
					0
				} else {
					// Move in the direction the ball is going, if known.
					match prev_ball_pos {
						Some(pbp) => if bp.0 < pbp.0 {
							-1
						} else {
							1
						},
						None => 0,
					}
				}
			};
			println!("Paddle command: {}", cmd);
			self.machine.input.enqueue(cmd);
			prev_ball_pos = Some(bp);
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
	
		for y in 0..self.ymax() + 1 {
			for x in 0..self.xmax() + 1 {
				print!("{}", self.screen.get(&(x, y)).unwrap_or(&' '));
			}
			println!("");
		}
	
		println!("{} blocks left", self.num_blocks());
		println!("Score: {}", self.score);
	}

	fn num_blocks(&self) -> usize {
		self.screen.values()
			.filter(|v| **v == '-')
			.count()
	}

	fn ball_pos(&self) -> (i64, i64) {
		self.screen.iter()
			.filter(|(_, v)| **v == '*')
			.map(|(k, _)| k)
			.cloned()
			.next()
			.unwrap()
	}

	fn paddle_pos(&self) -> (i64, i64) {
		self.screen.iter()
			.filter(|(_, v)| **v == '_')
			.map(|(k, _)| k)
			.cloned()
			.next()
			.unwrap()
	}

	fn xmax(&self) -> i64 {
		self.screen.keys().map(|(x, _)| *x).max().unwrap()
	}
	fn ymax(&self) -> i64 {
		self.screen.keys().map(|(_, y)| *y).max().unwrap()
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
