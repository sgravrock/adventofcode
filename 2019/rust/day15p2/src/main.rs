#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
mod bot;
mod machine_bot;
mod testing_bot;
mod pathfinder;
use machine_bot::MachineBot;
use pathfinder::shortest_path_len;

fn main() {
	let bot = MachineBot::new(input::puzzle_input());
	println!("{}", shortest_path_len(bot, (0, 0)));
}

