extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;

fn main() {
	println!("{}", first_coin("iwrupvqb"));
}

fn first_coin(secret: &str) -> i32 {
	let mut i = 0;

	while !is_valid_coin(secret, i) {
		i += 1
	}

	i
}

fn is_valid_coin(secret: &str, i: i32) -> bool {
	is_valid_hash(&make_hash(secret, i))
}

fn is_valid_hash(hash: &str) -> bool {
	hash.starts_with("000000")
}

#[test]
fn test_is_valid_hash() {
	assert_eq!(is_valid_hash("000001dbbfa"), false);
	assert_eq!(is_valid_hash("0000001dbbfa"), true);
}

fn make_hash(secret: &str, n: i32) -> String {
	let mut digest = Md5::new();
	let input = format!("{}{}", secret, n);
	digest.input_str(&input);
	digest.result_str()
}

#[test]
fn test_make_hash() {
	assert_eq!(make_hash("abcdef", 609043), "000001dbbfa3a5c83a2d506429c7b00e");
}
