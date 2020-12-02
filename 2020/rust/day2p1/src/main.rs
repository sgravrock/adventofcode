mod input;
extern crate regex;
use regex::Regex;
use std::ops::Range;

fn main() {
	println!("{:?}", solve(&parse(&input::puzzle_input())));
	// 506
}

#[derive(PartialEq, Eq, Debug)]
struct Password {
	count: Range<usize>,
	letter: char,
	password: String,
}

impl Password {
	fn is_valid(self: &Password) -> bool {
		let nc = self.password
			.chars()
			.filter(|c| *c == self.letter)
			.count();
		nc >= self.count.start && nc < self.count.end
	}
}

fn parse(input: &str) -> Vec<Password> {
	let re = Regex::new(r"(\d+)-(\d+) (.): (.*)$").unwrap();
	input.lines()
		.map(|line| {
			let caps = re.captures(line).unwrap();
			let min = caps.get(1).unwrap().as_str().parse::<usize>().unwrap();
			let max = caps.get(2).unwrap().as_str().parse::<usize>().unwrap();
			Password {
				count: min..(max + 1),
				letter: caps.get(3).and_then(|m| m.as_str().chars().next()).unwrap(),
				password: caps.get(4).unwrap().as_str().to_string()
			}
		})
		.collect()
}

#[test]
fn test_parse() {
	let input = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc";
	assert_eq!(parse(input), vec![
		Password { count: 1..4, letter: 'a', password: "abcde".to_string() },
		Password { count: 1..4, letter: 'b', password: "cdefg".to_string() },
		Password { count: 2..10, letter: 'c', password: "ccccccccc".to_string() },
	]);
}

fn solve(passwords: &Vec<Password>) -> usize {
	passwords.iter()
		.filter(|p| p.is_valid())
		.count()
}

#[test]
fn test_solve() {
	let passwords = parse("1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc");
	assert_eq!(solve(&passwords), 2);
}
