extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;

fn main() {
    println!("{:?}", find_nth_key("jlmsuwbz", 63));
}

fn find_nth_key(salt: &str, n: usize) -> (i32, String) {
	HashIter::new(0, salt)
		.filter(|h| is_key(&h))
		.nth(n)
		.map(|h| (h.index, h.hash))
		.unwrap()
}

#[test]
fn test_find_nth_key() {
	assert_eq!((39, "347dac6ee8eeea4652c7476d0f97bee5".to_string()),
		find_nth_key("abc", 0));
	assert_eq!((92, "ae2e85dd75d63e916a525df95e999ea0".to_string()),
		find_nth_key("abc", 1));
}

#[derive(PartialEq, Eq, Debug)]
struct Hash<'a> {
	index: i32,
	salt: &'a str,
	hash: String
}

impl <'a> Hash<'a> {
	fn new(index: i32, salt: &'a str, hash: String) -> Hash {
		Hash { index: index, salt: salt, hash: hash }
	}
}

struct HashIter<'a> {
	next_key: i32,
	salt: &'a str
}

impl <'a> HashIter<'a> {
	fn new(next_key: i32, salt: &str) -> HashIter {
		HashIter { next_key: next_key, salt: salt }
	}
}

impl <'a> Iterator for HashIter<'a> {
	type Item = Hash<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		let index = self.next_key;
		self.next_key += 1;
		Some(make_hash(self.salt, index))
	}
}

#[test]
fn test_hash_iter() {
	let iter = HashIter::new(0, "abc");
	let expected = vec![
		Hash::new(0, "abc", "577571be4de9dcce85a041ba0410f29f".to_string()),
		Hash::new(1, "abc", "23734cd52ad4a4fb877d8a1e26e5df5f".to_string())
	];
	let actual: Vec<Hash> = iter.take(2).collect();
	assert_eq!(expected, actual);
}

fn make_hash(salt: &str, n: i32) -> Hash {
	let mut digest = Md5::new();
	let input = format!("{}{}", salt, n);
	digest.input_str(&input);
	Hash::new(n, salt, digest.result_str())
}

#[test]
fn test_make_hash() {
	assert_eq!(make_hash("abcdef", 609043),
		Hash::new(609043, "abcdef", "000001dbbfa3a5c83a2d506429c7b00e".to_string()));
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

fn is_key(hash: &Hash) -> bool {
	match find_triplet(&hash.hash) {
		Some(c) => has_hash_with_quad(hash.index + 1, c, hash.salt),
		None => false
	}
}

fn has_hash_with_quad(start: i32, c: char, salt: &str) -> bool {
	let needle = format!("{}{}{}{}{}", c, c, c, c, c);
	HashIter::new(start, salt)
		.take(1000)
		.any(|h| h.hash.contains(&needle))
}

#[test]
fn test_is_key() {
	assert!(!is_key(&Hash::new(1, "abc", "23734cd52ad4a4fb877d8a1e26e5df5f".to_string()))); // no triplet
	assert!(!is_key(&Hash::new(18, "abc", "0034e0923cc38887a57bd7b1d4f953df".to_string() ))); // 888 but no 88888
	assert!(is_key(&Hash::new(39, "abc", "347dac6ee8eeea4652c7476d0f97bee5".to_string() )));
	assert!(!is_key(&Hash::new(45, "abc", "ddd37f736db183b6b4c186b87dd6236c".to_string() )));
}
