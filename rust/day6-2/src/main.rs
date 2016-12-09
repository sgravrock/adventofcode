use std::io::{self, BufRead};
use std::collections::HashMap;
extern crate itertools;
use itertools::Itertools;


fn main() {
	let stdin = io::stdin();
	let line_strings: Vec<String> = stdin.lock().lines()
		.map(|r| {
			match r {
				Ok(s) => s,
				Err(e) => panic!("Error reading stdin: {}", e)
			}
		}).collect();
	let lines = line_strings.iter().map(|s| s.as_str()).collect();
	let msg = find_msg(&lines);
	println!("{}", msg);
}

fn find_msg(src_msgs: &Vec<&str>) -> String {
	let cols: Vec<Vec<char>> = to_cols(src_msgs);
	cols.iter()
		.map(|col| least_frequent(col).unwrap())
		.collect()
}

#[test]
fn test_find_msg() {
	let input = vec![
		"eedadn",
		"drvtee",
		"eandsr",
		"raavrd",
		"atevrs",
		"tsrnev",
		"sdttsa",
		"rasrtv",
		"nssdts",
		"ntnada",
		"svetve",
		"tesnvt",
		"vntsnd",
		"vrdear",
		"dvrsen",
		"enarar",
	];
	assert_eq!(find_msg(&input), "advent");
}

fn to_cols(input: &Vec<&str>) -> Vec<Vec<char>> {
	// All input lines must be the same length.
	(0..input[0].len())
		.map(|col_ix| {
			input.iter()
				.map(|line| line.chars().nth(col_ix).unwrap())
				.collect()
		})
		.collect()
}

#[test]
fn test_to_cols() {
	let input = vec!["ab", "cd"];
	let expected = vec![vec!['a', 'c'], vec!['b', 'd']];
	assert_eq!(to_cols(&input), expected);
}

fn find_freqs(chars: &Vec<char>) -> HashMap<char, i32> {
	let mut freqs = HashMap::new();

	for cr in chars {
		let c = *cr;
		if c != '-' {
			let n = match freqs.get(&c) {
				Some(x) => x + 1,
				None => 1
			};
			freqs.insert(c, n);
		}
	}

	freqs
}

#[test]
fn test_find_freqs() {
	let word = vec!['a', 'b', 'c', 'a', 'b', 'a', '-'];
	let freqs = find_freqs(&word);
	assert_eq!(freqs.get(&'a'), Some(&3));
	assert_eq!(freqs.get(&'b'), Some(&2));
	assert_eq!(freqs.get(&'c'), Some(&1));
	assert_eq!(freqs.get(&'-'), None);
}

fn least_frequent(chars: &Vec<char>) -> Option<char> {
	let freqs = find_freqs(chars);
	freqs
			.iter()
			.sorted_by(|a, b| Ord::cmp(a.1, b.1))
			.first()
			.map(|r| *(r.0))
}

#[test]
fn test_least_frequent() {
	let abca = vec!['b', 'b', 'c'];
	assert_eq!(least_frequent(&abca), Some('c'));;
	let empty: Vec<char> = vec![];
	assert_eq!(least_frequent(&empty), None);
}
