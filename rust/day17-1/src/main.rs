extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;
use std::collections::VecDeque;
use std::rc::Rc;

fn main() {
	println!("{:?}", shortest_path("pslxynzg"));
}

fn shortest_path(passcode: &str) -> Option<String> {
	let mut queue = VecDeque::new();
	queue.push_back(Rc::new(Node { pos: (0,0), prev: None }));

	while queue.len() > 0 {
		let node = queue.pop_front().unwrap();
		let path = node.path();

		if node.pos == (3,3) {
			return Some(path);
		}

		for d in next_directions(passcode, node.pos, &path) {
			queue.push_back(Rc::new(Node {
				pos: next_pos(node.pos, d),
				prev: Some((d, node.clone()))
			}));
		}
	}

	None
}

struct Node {
	pos: (i32, i32),
	prev: Option<(char, Rc<Node>)>
}

impl Node {
	fn path(&self) -> String {
		let mut s = String::new();
		self.path_into(&mut s);
		s
	}

	fn path_into(&self, s: &mut String) {
		match self.prev {
			Some((d, ref p)) => {
				p.path_into(s);
				s.push(d);
			},
			None => {}
		}
	}
}

#[test]
fn test_shortest_path() {
	assert_eq!(Some("DDRRRD".to_string()), shortest_path("ihgpwlah"));
	assert_eq!(Some("DDUDRLRRUDRD".to_string()), shortest_path("kglvqrro"));
	assert_eq!(Some("DRURDRUDDLLDLUURRDULRLDUUDDDRR".to_string()),
		shortest_path("ulqzkmiv"));

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
