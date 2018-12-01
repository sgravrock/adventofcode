fn main() {
	let input = "Disc #1 has 17 positions; at time=0, it is at position 15
Disc #2 has 3 positions; at time=0, it is at position 2
Disc #3 has 19 positions; at time=0, it is at position 4
Disc #4 has 13 positions; at time=0, it is at position 2
Disc #5 has 7 positions; at time=0, it is at position 2
Disc #6 has 5 positions; at time=0, it is at position 0";
   println!("{}", min_time(&parse_input(input)));
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Disc {
	positions: i32,
	start: i32,
}

impl Disc {
	fn position_at_time(&self, time: i32) -> i32 {
		(self.start + time) % self.positions
	}
}

fn parse_input(input: &str) -> Vec<Disc> {
	input.lines()
		.map(|line| {
			let tokens: Vec<&str> = line.split(' ').collect();
			let p = tokens[3].parse::<i32>().unwrap();
			let s = tokens[11].parse::<i32>().unwrap();
			Disc { positions: p, start: s }
		})
		.collect()
}

#[test]
fn test_parse_input() {
	let input = "Disc #1 has 5 positions; at time=0, it is at position 4
Disc #2 has 2 positions; at time=0, it is at position 1";
	let expected = vec![
		Disc { positions: 5, start: 4 },
		Disc { positions: 2, start: 1 },
	];
	assert_eq!(expected, parse_input(input));
}

fn min_time(discs: &Vec<Disc>) -> i32 {
	let mut time = 0;

	while !can_pass_discs(time, &discs) {
		time += 1;
	}

	time
}

#[test]
fn test_min_time() {
	let discs = parse_input("Disc #1 has 5 positions; at time=0, it is at position 4
Disc #2 has 2 positions; at time=0, it is at position 1");
	assert_eq!(5, min_time(&discs));
}

fn can_pass_discs(time: i32, discs: &Vec<Disc>) -> bool {
	for i in 0..discs.len() {
		let disc_time = (time + i as i32) + 1;
		if discs[i].position_at_time(disc_time) != 0 {
			return false;
		}
	}

	true
}
