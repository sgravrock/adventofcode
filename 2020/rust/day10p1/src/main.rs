mod input;
use itertools::Itertools;
use itertools::chain;

fn main() {
	let adapters: Vec<u32> = input::puzzle_input()
		.lines()
		.map(|line| line.parse().unwrap())
		.collect();
	let (ones, threes) = differences(&adapters);
	println!("{}", ones * threes);
	// 1836
}

fn differences(adapters: &Vec<u32>) -> (u32, u32) {
	let mut ones = 0;
	let mut threes = 1; // account for built-in adapter
	let adjacent_pairs = chain(&[0], adapters).sorted().tuple_windows();

	for (x, y) in adjacent_pairs {
		if y - x == 1 {
			ones += 1;
		} else if y - x == 3 {
			threes += 1;
		}
	}

	(ones, threes)
}

#[test]
fn test_differences() {
	let adapters = vec![16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4];
	assert_eq!(differences(&adapters), (7, 5));
}
