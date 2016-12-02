use std::collections::HashSet;

#[derive(PartialEq, Debug, Copy, Clone)]
struct Birange {
	xs: i32,
	xe: i32,
	ys: i32,
	ye: i32,
}

#[derive(PartialEq, Debug, Copy, Clone)]
enum CmdType {
	On,
	Off,
	Toggle
}

#[derive(PartialEq, Debug, Copy, Clone)]
struct Cmd {
	t: CmdType,
	range: Birange
}

fn main() {
    println!("Hello, world!");
}

fn handle_cmd(grid: &mut HashSet<[i32;2]>, cmd: Cmd) {
	match cmd.t {
		CmdType::On => {
			for x in cmd.range.xs..cmd.range.xe {
				for y in cmd.range.ys..cmd.range.ye {
					grid.insert([x, y]);
				}
			}
		},
		CmdType::Off => {
			for x in cmd.range.xs..cmd.range.xe {
				for y in cmd.range.ys..cmd.range.ye {
					grid.remove(&[x, y]);
				}
			}
		},
		CmdType::Toggle => {
			for x in cmd.range.xs..cmd.range.xe {
				for y in cmd.range.ys..cmd.range.ye {
					let c = [x, y];

					if grid.contains(&c) {
						grid.remove(&c);
					} else {
						grid.insert(c);
					}
				}
			}
		},
	}
}

#[test]
fn handle_cmd_on() {
	let mut grid: HashSet<[i32;2]> = HashSet::new();
	handle_cmd(&mut grid, parse_cmd("turn on 0,0 through 0,1"));
	assert_eq!(grid.len(), 2);

	handle_cmd(&mut grid, parse_cmd("turn on 0,0 through 999,999"));
	assert_eq!(grid.len(), 1000000);
}

#[test]
fn handle_cmd_off() {
	let mut grid: HashSet<[i32;2]> = HashSet::new();
	for x in 0..4 {
		for y in 0..4 {
			grid.insert([x, y]);
		}
	}

	handle_cmd(&mut grid, parse_cmd("turn off 1,1 through 2,2"));
	assert_eq!(grid.len(), 12);
}

#[test]
fn handle_cmd_toggle() {
	let mut grid: HashSet<[i32;2]> = HashSet::new();
	grid.insert([0, 0]);
	handle_cmd(&mut grid, parse_cmd("toggle 0,0 through 1,1"));
	assert_eq!(grid.len(), 3);
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
