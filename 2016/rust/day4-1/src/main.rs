extern crate itertools;
use itertools::Itertools;
use std::collections::HashMap;
use std::io::{self, Read};


fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	let rooms = parse_input(&input);
	let result = rooms.iter()
		.filter(|r| is_real(&r))
		.map(|r| r.sector_id)
		.sum::<i32>();
	println!("{}", result);
}

#[derive(PartialEq, Debug)]
struct Room {
	enc_name: String,
	sector_id: i32,
	checksum: String,
}

fn is_delim(c: char) -> bool {
	c == '\n' || c == '-' || c == '[' || c == ']'
}

fn parse_input(input: &str) -> Vec<Room> {
	let lines = input.split("\n");
	let mut result: Vec<Room> = Vec::new();

	for line in lines {
		let mut tokens: Vec<&str> = line.split(is_delim).collect();
		tokens.pop(); // consume trailing ""
		let sum = tokens.pop().unwrap();
		let sector = tokens.pop().unwrap().parse::<i32>().unwrap();
		let name = tokens.join("-");
		result.push(Room {
			enc_name: name,
			sector_id: sector,
			checksum: String::from(sum)
		})
	}

	result
}

#[test]
fn test_parse_input() {
	let input = "bkwzkqsxq-tovvilokx-nozvyiwoxd-172[fstek]
wifilzof-wbiwifuny-yhachyylcha-526[qrazx]";
	let result = parse_input(input);
	let expected = vec![
		Room { 
			enc_name: String::from("bkwzkqsxq-tovvilokx-nozvyiwoxd"),
			sector_id: 172,
			checksum: String::from("fstek")
		},
		Room {
			enc_name: String::from("wifilzof-wbiwifuny-yhachyylcha"),
			sector_id: 526,
			checksum: String::from("qrazx")
		}
	];
	assert_eq!(result, expected);
}

fn expected_checksum(enc_name: &str) -> String {
	let freqs = find_freqs(enc_name);
	let sorted = freqs.iter()
		.sorted_by(|a, b| {
			if a.1 == b.1 {
				Ord::cmp(a.0, b.0)
			} else {
				Ord::cmp(b.1, a.1)
			}
		});
	let top_5 = sorted.iter()
		.take(5)
		.map(|&(k, _)| *k);
	top_5.collect::<String>()
}

#[test]
fn test_expected_checksum() {
	assert_eq!(expected_checksum("aaaaa-bbb-z-y-x"), "abxyz")
}

fn find_freqs(s: &str) -> HashMap<char, i32> {
	let mut freqs = HashMap::new();

	for c in s.chars() {
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
	let freqs = find_freqs("abcaba-");
	assert_eq!(freqs.get(&'a'), Some(&3));
	assert_eq!(freqs.get(&'b'), Some(&2));
	assert_eq!(freqs.get(&'c'), Some(&1));
	assert_eq!(freqs.get(&'-'), None);
}

fn is_real(room: &Room) -> bool {
	expected_checksum(&room.enc_name) == room.checksum
}

#[test]
fn test_is_real() {
	assert_eq!(is_real(&Room {
			enc_name: String::from("aaaaa-bbb-z-y-x"),
			checksum: String::from("abxyz"),
			sector_id: 0
		}),
		true);
	assert_eq!(is_real(&Room {
			enc_name: String::from("totally-real-roomt"),
			checksum: String::from("decoy"),
			sector_id: 0
		}),
		false);
}
