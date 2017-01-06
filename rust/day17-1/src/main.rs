extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;

fn main() {
}

/*
fn shortest_path(passcode: &str) -> Option<String> {
	bfs((0,0), |p| p == (3,3), next_directions)
		.map(|path| path.collect());
}

#[test]
fn test_shortest_path() {
	assert_eq!("DDRRRD", shortest_path("ihgpwlah"));
	assert_eq!("DDUDRLRRUDRD", shortest_path("kglvqrro"));
	assert_eq!("DRURDRUDDLLDLUURRDULRLDUUDDDRR", shortest_path("ulqzkmiv"));

}
*/

fn position(path: &[char]) -> (i32, i32) {
	path.iter().fold((0,0), next_pos)
}

fn next_pos(start: (i32, i32), dir: &char) -> (i32, i32) {
	match *dir {
		'U' => (start.0, start.1 - 1),
		'D' => (start.0, start.1 + 1),
		'L' => (start.0 - 1, start.1),
		'R' => (start.0 + 1, start.1),
		_ => panic!("Bad direction: {}", dir)
	}
}

#[test]
fn test_position() {
	assert_eq!((0,0), position(&vec![]));
	assert_eq!((1,0), position(&vec!['R']));
	assert_eq!((1,1), position(&vec!['R', 'D']));
	assert_eq!((0,1), position(&vec!['R', 'D', 'L']));
	assert_eq!((0,0), position(&vec!['R', 'D', 'L', 'U']));
}

fn next_directions(passcode: &str, position: (i32, i32), path: &[char]) -> Vec<char> {
	let dirs = vec!['U', 'D', 'L', 'R'];
	let suffix: String = path.iter().cloned().collect();
	let hash = make_hash(&(passcode.to_string() + &suffix));
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
				('L', (0, _)) => false,
				_ => true
			}
		})
		.collect()
}



#[test]
fn test_next_directions() {
	assert_eq!(vec!['D'], next_directions("hijkl", (0,0), &vec![]));
	assert_eq!(vec!['U', 'R'], next_directions("hijkl", (0,1), &vec!['D']));
	assert_eq!(vec!['R'], next_directions("hijkl", (0,1), &vec!['D', 'U']));
	assert_eq!(0, next_directions("hijkl", (1,0), &vec!['D', 'U', 'R']).len());
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
