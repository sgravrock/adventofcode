extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;
use std::collections::HashMap;

fn main() {
	 let mut hasher = CachingHasher::new("jlmsuwbz");
    println!("{:?}", find_nth_key(64, &mut hasher));
}

fn find_nth_key(n: usize, hasher: &mut CachingHasher) -> (i32, String) {
	let mut nfound = 0;

	for i in 0.. {
		let h = hasher.make_hash(i);

		if is_key(&h, hasher) {
			nfound += 1;

			if nfound == n {
				return (h.index, h.hash.clone());
			}
		}
	}

	panic!("Ran out of integers");
}

#[test]
fn test_find_nth_key() {
	let mut hasher = CachingHasher::new("jlmsuwbz");
	assert_eq!((22429, "922b7bd4af8e0dcc5a4ddd7eaef72569".to_string()),
		find_nth_key(64, &mut hasher));
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Hash {
	index: i32,
	hash: String
}

impl Hash {
	fn new(index: i32, hash: String) -> Hash {
		Hash { index: index, hash: hash }
	}
}

struct CachingHasher<'a> {
	salt: &'a str,
	storage: HashMap<i32, Hash>
}

impl <'a> CachingHasher<'a> {
	fn new(salt: &'a str) -> CachingHasher {
		CachingHasher { salt: salt, storage: HashMap::new() }
	}

	fn make_hash(&mut self, index: i32) -> Hash {
		let salt = self.salt;
		self.storage.entry(index)
			.or_insert_with(|| make_hash(salt, index))
			.clone()
	}
}

fn make_hash(salt: &str, n: i32) -> Hash {
	let input = format!("{}{}", salt, n);
	Hash::new(n, md5(&input))
}

fn md5(input: &str) -> String {
	let mut s = input.to_string();

	for _ in 0..2017 {
		let mut digest = Md5::new();
		digest.input_str(&s);
		s = digest.result_str()
	}

	s
}

fn find_triplet(input: &str) -> Option<char> {
	// This should be a regex, but Rust regexes don't support backreferences,
	// lookahead, or repetition of wildcards (.{3}).
	let chars: Vec<char> = input.chars().collect();

	for i in 2..chars.len() {
		if chars[i] == chars[i - 1] && chars[i] == chars[i - 2] {
			return Some(chars[i]);
		}
	}

	None
}

#[test]
fn test_find_triplet() {
	assert_eq!(None, find_triplet("abbcdefg"));
	assert_eq!(Some('b'), find_triplet("abbbbfg"));
}

fn is_key(hash: &Hash, hasher: &mut CachingHasher) -> bool {
	match find_triplet(&hash.hash) {
		Some(c) => has_hash_with_quad(hash.index + 1, c, hasher),
		None => false
	}
}

fn has_hash_with_quad(start: i32, c: char, hasher: &mut CachingHasher) -> bool {
	let needle = format!("{}{}{}{}{}", c, c, c, c, c);

	for i in 1..1001 {
		if hasher.make_hash(start + i).hash.contains(&needle) {
			return true;
		}
	}

	false
}
