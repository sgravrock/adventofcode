mod input;

fn main() {
	let solution = solve(&input::puzzle_input());
	println!("{:?}", solution);
	// 964875
}

fn solve(entries: &Vec<u32>) -> Option<u32> {
	for i in 0..entries.len() {
		for j in 0..entries.len() {
			if i != j && entries[i] + entries[j] == 2020 {
				return Some(entries[i] * entries[j])
			}
		}
	}

	None
}

#[test]
fn test_solve() {
	let input = vec![1721, 979, 366, 299, 675, 1456];
	assert_eq!(solve(&input), Some(514579));
}
