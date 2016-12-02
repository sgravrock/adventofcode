use std::collections::HashMap;
use std::io;
use std::io::prelude::*;
use std::cmp::max;


#[derive(PartialEq, Debug)]
struct Birange {
	xs: i32,
	xe: i32,
	ys: i32,
	ye: i32,
}

#[derive(PartialEq, Debug)]
enum CmdType {
	On,
	Off,
	Toggle
}

#[derive(PartialEq, Debug)]
struct Cmd {
	t: CmdType,
	range: Birange
}

fn main() {
	let stdin = io::stdin();
	let cmds = stdin.lock().lines()
		.map(|s| parse_cmd(&s.unwrap()));

	let mut grid: HashMap<[i32;2], i32> = HashMap::new();

	for cmd in cmds {
		handle_cmd(&mut grid, cmd);
	}

	println!("{}", total_brightness(&grid));
}

fn total_brightness(grid: &HashMap<[i32;2], i32>) -> i32 {
	grid.values().sum()
}

#[test]
fn test_total_brightness() {
	let mut grid: HashMap<[i32;2], i32> = HashMap::new();
	grid.insert([0, 0], 2);
	grid.insert([0, 1], 3);
	assert_eq!(total_brightness(&grid), 5);
}

fn handle_cmd(grid: &mut HashMap<[i32;2], i32>, cmd: Cmd) {
	for x in cmd.range.xs..cmd.range.xe {
		for y in cmd.range.ys..cmd.range.ye {
			let c = [x, y];
			let default = 0;
			let old_brightness = *(grid.get(&c).unwrap_or(&default));
			let new_brightness = match cmd.t {
				CmdType::On => old_brightness + 1,
				CmdType::Off => max(0, old_brightness - 1),
				CmdType::Toggle => old_brightness + 2
			};
			grid.insert(c, new_brightness);
		}
	}
}

#[test]
fn handle_cmd_on() {
	let mut grid: HashMap<[i32;2], i32> = HashMap::new();
	handle_cmd(&mut grid, parse_cmd("turn on 0,0 through 0,1"));
	assert_eq!(total_brightness(&grid), 2);

	handle_cmd(&mut grid, parse_cmd("turn on 0,0 through 999,999"));
	assert_eq!(total_brightness(&grid), 1000002);
}

#[test]
fn handle_cmd_off() {
	let mut grid: HashMap<[i32;2], i32> = HashMap::new();
	for x in 0..4 {
		for y in 0..4 {
			grid.insert([x, y], 2);
		}
	}

	assert_eq!(total_brightness(&grid), 32);
	handle_cmd(&mut grid, parse_cmd("turn off 1,1 through 2,2")); // to 1
	assert_eq!(total_brightness(&grid), 28);
	handle_cmd(&mut grid, parse_cmd("turn off 1,1 through 2,2")); // to 0
	assert_eq!(total_brightness(&grid), 24);
	handle_cmd(&mut grid, parse_cmd("turn off 1,1 through 2,2")); // to 0
	assert_eq!(total_brightness(&grid), 24);
}

#[test]
fn handle_cmd_toggle() {
	let mut grid: HashMap<[i32;2], i32> = HashMap::new();
	grid.insert([0, 0], 1);
	handle_cmd(&mut grid, parse_cmd("toggle 0,0 through 1,1"));
	assert_eq!(total_brightness(&grid), 9);
}


fn parse_cmd(s: &str) -> Cmd {
	let words: Vec<&str> = s.split_whitespace().collect();
	let cmd_type = match words[0] {
		"turn" => match words[1] {
			"on" => CmdType::On,
			"off" => CmdType::Off,
			_ => panic!("Unrecognized command {}", s)
		},
		"toggle" => CmdType::Toggle,
		_ => panic!("Unrecognized command {}", s)
	};

	let start_ix = match cmd_type {
		CmdType::Toggle => 1,
		_ => 2
	};

	Cmd {
		t: cmd_type,
		range: parse_birange(words[start_ix], words[start_ix + 2])
	}
}

fn parse_birange(start: &str, end: &str) -> Birange {
	let start_pair = parse_ipair(start);
	let end_pair = parse_ipair(end);
	Birange {
		xs: start_pair[0], xe: end_pair[0] + 1,
		ys: start_pair[1], ye: end_pair[1] + 1,
	}
}

fn parse_ipair(s: &str) -> [i32;2] {
	let words: Vec<&str> = s.split(",").collect();
	[
		words[0].parse::<i32>().unwrap(),
		words[1].parse::<i32>().unwrap(),
	]
}

#[test]
fn test_parse_cmd() {
	let on = parse_cmd("turn on 0,0 through 999,999");
	assert_eq!(Cmd { t: CmdType::On, range: Birange { xs: 0, xe: 1000, ys: 0, ye: 1000 }}, on);
	let off = parse_cmd("turn off 499,499 through 500,500");
	assert_eq!(Cmd { t: CmdType::Off, range: Birange { xs: 499, xe: 501, ys: 499, ye: 501 }}, off);
	let toggle = parse_cmd("toggle 0,0 through 999,0");
	assert_eq!(Cmd { t: CmdType::Toggle, range: Birange { xs: 0, xe: 1000, ys: 0, ye: 1 }}, toggle);
}
