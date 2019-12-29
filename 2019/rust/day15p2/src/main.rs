#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
mod bot;
mod machine_bot;
mod testing_bot;
mod pathfinder;
mod map;
use machine_bot::MachineBot;
use pathfinder::minutes_to_fill;

fn main() {
	let bot = MachineBot::new(input::puzzle_input());
	// 341 is too large
	println!("{}", minutes_to_fill(bot));
}

