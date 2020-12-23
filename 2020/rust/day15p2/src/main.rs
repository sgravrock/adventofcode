use std::collections::HashMap;

fn main() {
	println!("{}", solve(vec![18,8,0,5,4,1,20], 30000000));
	// 13710
}

#[derive(Debug, Clone)]
struct NumInfo {
	last_spoken_ix: usize,
	prev_spoken_ix: Option<usize>,
}

impl NumInfo {
	fn new(last_spoken_ix: usize) -> NumInfo {
		NumInfo { last_spoken_ix, prev_spoken_ix: None }
	}

	fn distance(&self) -> Option<usize> {
		self.prev_spoken_ix.map(|pi| self.last_spoken_ix - pi)
	}

	fn update(&mut self, i: usize) {
		self.prev_spoken_ix = Some(self.last_spoken_ix);
		self.last_spoken_ix = i;
	}
}

fn solve(input: Vec<u32>, n_turns: usize) -> u32 {
	let mut infos: HashMap<u32, NumInfo> = HashMap::new();
	let mut last_spoken = input[input.len() - 1];

	for i in 0..input.len() {
		infos.insert(input[i], NumInfo::new(i));
	}

	for i in input.len()..n_turns {
		let speak = infos[&last_spoken].distance().unwrap_or(0) as u32;
		infos.entry(speak)
			.and_modify(|e| e.update(i))
			.or_insert_with(|| NumInfo::new(i));
		last_spoken = speak;
	}

	last_spoken
}

#[test]
fn test_solve() {
	assert_eq!(solve(vec![0,3,6], 30000000), 175594);
}
