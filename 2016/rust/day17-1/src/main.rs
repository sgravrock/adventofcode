extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;
use std::collections::VecDeque;
use std::rc::Rc;
use std::cmp;

fn main() {
	println!("{:?}", longest_path("pslxynzg", ""));
}

fn longest_path(passcode: &str, prefix: &str) -> Option<usize> {
	let pos = position(prefix);
	
	if pos == (0, 0) {
		return Some(prefix.chars().count());
	}

	next_directions(passcode, pos, prefix)
		.filter_map(|path| longest_path(passcode, format!("{}{}", prefix, d)))
		.max()
		.map(|n| n + 1)
}

fn position(path: &[char]) -> (i32, i32) {
	path.iter().fold((0,0), next_pos)
}


#[test]
fn test_longest_path() {
	assert_eq!(Some(370), longest_path("ihgpwlah"));
	assert_eq!(Some(492), longest_path("kglvqrro"));
	assert_eq!(Some(830), longest_path("ulqzkmiv"));

}

fn next_pos(start: (i32, i32), dir: char) -> (i32, i32) {
	match dir {
		'U' => (start.0, start.1 - 1),
		'D' => (start.0, start.1 + 1),
		'L' => (start.0 - 1, start.1),
		'R' => (start.0 + 1, start.1),
		_ => panic!("Bad direction: {}", dir)
	}
}

fn next_directions(passcode: &str, position: (i32, i32), path: &str) -> Vec<char> {
	let dirs = vec!['U', 'D', 'L', 'R'];
	let hash = make_hash(&(passcode.to_string() + &path));
	let chars = hash.chars().take(4);

	dirs.into_iter()
		.zip(chars).
		filter_map(|(d, hc)| {
			match hc {
				'b' => Some(d),
				'c' => Some(d),
				'd' => Some(d),
				'e' => Some(d),
				'f' => Some(d),
				_ => None
			}
		})
		.filter(|d| {
			match (*d, position) {
				('U', (_, 0)) => false,
				('D', (_, 3)) => false,
				('L', (0, _)) => false,
				('R', (3, _)) => false,
				_ => true
			}
		})
		.collect()
}



#[test]
fn test_next_directions() {
	assert_eq!(vec!['D'], next_directions("hijkl", (0,0), ""));
	assert_eq!(vec!['U', 'R'], next_directions("hijkl", (0,1), "D"));
	assert_eq!(vec!['R'], next_directions("hijkl", (0,1), "DU"));
	assert_eq!(0, next_directions("hijkl", (1,0), "DUR").len());
}

fn make_hash(input: &str) -> String {
	let mut digest = Md5::new();
	digest.input_str(&input);
	digest.result_str()
}

#[test]
fn test_make_hash() {
	assert_eq!("e80b5017098950fc58aad83c8c14978e", make_hash("abcdef"));
}
