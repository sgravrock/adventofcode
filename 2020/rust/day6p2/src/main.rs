mod input;
use std::collections::HashSet;

fn main() {
    println!("{}", solve(input::puzzle_input()));
	 // 3122
}

fn solve(input: &str) -> usize {
	input.split("\n\n")
		.map(|group_input| {
			let mut people: Vec<HashSet<char>> = group_input.lines()
				.map(|line| line.chars().collect())
				.collect();

			let mut intersection = people.pop().unwrap();

			for p in people {
				intersection = intersection.into_iter()
					.filter(|c| p.contains(c))
					.collect();
			}

			intersection.len()
		})
		.sum()
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
