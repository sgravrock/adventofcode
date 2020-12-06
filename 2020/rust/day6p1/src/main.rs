mod input;
use itertools::Itertools;

fn main() {
    println!("{}", solve(input::puzzle_input()));
	 // 6683
}

fn solve(input: &str) -> usize {
	input.split("\n\n")
		.map(|group_input| {
			group_input.chars()
				.filter(|c| *c != '\n')
				.unique()
				.count()
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
	assert_eq!(solve(input), 11);
}
