use std::io::{self, Read};
use std::collections::HashMap;
use std::cmp::{min, max};

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	let config = parse_input(&input);
	let outputs = simulate(config);
	let n = [0, 1, 2].iter()
		.map(|i| outputs.get(i).unwrap())
		.fold(1, |acc, n| acc * n);
	println!("{}", n);
}

#[derive(PartialEq, Debug, Clone)]
struct Config {
	assigns: Vec<Assignment>,
	bots: Vec<BotConfig>,
}

#[derive(PartialEq, Debug, Clone)]
struct Assignment {
	value: i32,
	bot: i32,
}

#[derive(PartialEq, Debug, Clone)]
struct BotConfig {
	id: i32,
	low: Destination,
	high: Destination
}

#[derive(PartialEq, Debug, Clone)]
enum Destination {
	Bot(i32),
	Output(i32)
}

fn parse_input(input: &str) -> Config {
	let mut assigns: Vec<Assignment> = vec![];
	let mut bots: Vec<BotConfig> = vec![];

	for line in input.split("\n") {
		let words: Vec<&str> = line.split(" ").collect();
			
		match words[0] {
			"value" => assigns.push(parse_assignment(&words)),
			"bot" => bots.push(parse_bot(&words)),
			_ => panic!("Don't know how to parse line: {}", line)
		}
	}

	Config { assigns: assigns, bots: bots }
}

fn parse_assignment(words: &Vec<&str>) -> Assignment {
	Assignment {
		value: words[1].parse::<i32>().unwrap(),
		bot: words[5].parse::<i32>().unwrap(),
	}
}

fn parse_bot(words: &Vec<&str>) -> BotConfig {
	BotConfig {
		id: words[1].parse::<i32>().unwrap(),
		low: parse_dest(words[5], words[6]),
		high: parse_dest(words[10], words[11]),
	}
}

fn parse_dest(t: &str, snum: &str) -> Destination {
	let num = snum.parse::<i32>().unwrap();
	match t {
		"bot" => Destination::Bot(num),
		"output" => Destination::Output(num),
		_ => panic!("Unrecognized dest type {}", t)
	}
}


struct Move {
	chip: i32,
	dest: Destination,
}

struct Bot {
	config: BotConfig,
	chips: Vec<i32>
}

impl Bot {
	fn take(&mut self, chip: i32) {
		if self.chips.len() > 1 {
			panic!("Bot {} tried to take a third chip: {}", self.config.id, chip);
		}

		self.chips.push(chip);
	}

	fn give(&mut self) -> Option<[Move; 2]> {
		if self.chips.len() != 2 {
			return None;
		}

		let low = min(self.chips[0], self.chips[1]);
		let high = max(self.chips[0], self.chips[1]);
		self.chips = vec![];
		Some([
			Move {
				chip: low,
				dest: self.config.low.clone()
			},
			Move {
				chip: high,
				dest: self.config.high.clone()
			},
		])
	}
}

struct Factory {
	bots: HashMap<i32, Bot>,
	outputs: HashMap<i32, i32>,
}

impl Factory {
	fn give_to_bot(&mut self, chip: i32, bot_id: i32) {
		let mut bot = self.bots.get_mut(&bot_id).unwrap();
		bot.take(chip);
	}

	fn do_next_moves(&mut self) -> bool {
		match self.find_next_moves() {
			None => false,
			Some(moves) => {
				for m in moves.iter() {
					match m.dest {
						Destination::Bot(id) => {
							let mut bot = self.bots.get_mut(&id).unwrap();
							bot.take(m.chip);
							0
						},
						Destination::Output(n) => {
							self.outputs.insert(n, m.chip);
							0
						}
					};
				}
				true
			}
		}
	}

	fn find_next_moves(&mut self) -> Option<[Move; 2]> {
		for (_, bot) in self.bots.iter_mut() {
			let moves = bot.give();

			if moves.is_some() {
				return moves;
			}
		}

		None
	}
}

// I couldn't resist.
fn factory_factory(config: Config) -> Factory {
	let mut bots = HashMap::new();

	for bc in config.bots {
		bots.insert(bc.id, Bot { config: bc, chips: vec![] });
	}

	let mut factory = Factory { bots: bots, outputs: HashMap::new() };

	for a in config.assigns {
		factory.give_to_bot(a.value, a.bot);
	}
	
	factory
}

#[test]
fn test_parse_input() {
	let input = "value 5 goes to bot 2
bot 1 gives low to output 1 and high to bot 0";
	let expected = Config {
		assigns: vec![Assignment { value: 5, bot: 2 }],
		bots: vec![BotConfig { id: 1, low: Destination::Output(1), high: Destination::Bot(0) }],
	};
	assert_eq!(parse_input(input), expected);
}


fn simulate(config: Config) -> HashMap<i32, i32> {
	let mut factory = factory_factory(config);
	while factory.do_next_moves() { }
	factory.outputs
}
