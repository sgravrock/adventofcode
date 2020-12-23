fn main() {
	println!("{}", solve(vec![18,8,0,5,4,1,20], 2020));
}

fn solve(input: Vec<u32>, n_turns: usize) -> u32 {
	let mut spoken = input.clone();

	for _ in input.len()..n_turns {
		let say = distance(&spoken).unwrap_or(0);
		spoken.push(say);
	}

	spoken[spoken.len() - 1]
}

fn distance(spoken: &Vec<u32>) -> Option<u32> {
	let lastnum = spoken[spoken.len() - 1];

	for i in (0..spoken.len() - 1).rev() {
		if spoken[i] == lastnum {
			return Some((spoken.len() - i - 1) as u32);
		}
	}

	None
}

#[test]
fn test_solve() {
	assert_eq!(solve(vec![0,3,6], 10), 0);
	assert_eq!(solve(vec![1,3,2], 2020), 1);
	assert_eq!(solve(vec![2,1,3], 2020), 10);
}
