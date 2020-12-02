mod input;
extern crate regex;
use regex::Regex;

fn main() {
	println!("{:?}", solve(&parse(&input::puzzle_input())));
	// 443
}

#[derive(PartialEq, Eq, Debug)]
struct Password {
	p1: usize,
	p2: usize,
	letter: char,
	password: String,
}

impl Password {
	fn is_valid(self: &Password) -> bool {
		let chars: Vec<char> = self.password.chars().collect();
		(chars[self.p1 - 1] == self.letter) != (chars[self.p2 - 1] == self.letter)
	}
}

fn parse(input: &str) -> Vec<Password> {
	let re = Regex::new(r"(\d+)-(\d+) (.): (.*)$").unwrap();
	input.lines()
		.map(|line| {
			let caps = re.captures(line).unwrap();
			Password {
				p1: caps.get(1).unwrap().as_str().parse::<usize>().unwrap(),
				p2: caps.get(2).unwrap().as_str().parse::<usize>().unwrap(),
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
		Password { p1: 1, p2: 3, letter: 'a', password: "abcde".to_string() },
		Password { p1: 1, p2: 3, letter: 'b', password: "cdefg".to_string() },
		Password { p1: 2, p2: 9, letter: 'c', password: "ccccccccc".to_string() },
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
	assert_eq!(solve(&passwords), 1);
}
