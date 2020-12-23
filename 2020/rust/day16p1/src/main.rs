use std::ops::RangeInclusive;
mod input;

fn main() {
	println!("{}", solve(input::puzzle_input()));
	// 25961
}

fn solve(input: &str) -> u32 {
	let mut paragraphs = input.split("\n\n");
	let rules = parse_rules(paragraphs.next().unwrap());
	paragraphs.next(); // skip own ticket
	let tickets = paragraphs.next().unwrap();

	tickets
		.replace("nearby tickets:\n", "")
		.replace("\n", ",")
		.split(",")
		.map(|s| s.parse::<u32>().unwrap())
		.filter(|n| !rules.iter().any(|rule| rule.contains(n)))
		.sum()
}

fn parse_rules(input: &str) -> Vec<RangeInclusive<u32>> {
	input.lines()
		.flat_map(|line| {
			line.split(": ").skip(1).next().unwrap()
				.split(" or ")
				.map(|rs| {
					let mut nums = rs.split("-")
						.map(|ns| ns.parse::<u32>().unwrap());
					nums.next().unwrap()..=nums.next().unwrap()
				})
		})
		.collect()
}

#[test]
fn test_solve() {
	let input = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12";
	assert_eq!(solve(input), 71);
}
