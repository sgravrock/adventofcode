mod input;
use std::collections::HashSet;

fn main() {
    println!("{}", solve(input::puzzle_input()));
	 // 3122
}

fn solve(input: &str) -> usize {
	input.split("\n\n")
		.map(|group_input| {
			let people = group_input.lines()
				.map(|line| line.chars().collect::<HashSet<char>>());
			intersection(people).len()
		})
		.sum()
}

fn intersection<T: Iterator<Item = HashSet<char>>>(mut iter: T) -> HashSet<char> {
	let first = iter.next().unwrap();
	iter.fold(first, |acc, p| &p & &acc)
}

#[test]
fn test_solve() {
	let input = "abc

a
b
c

ab
ac

a
a
a
a

b";
	assert_eq!(solve(input), 6);
}
