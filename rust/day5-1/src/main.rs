extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;


fn main() {
    println!("{}", find_password("ojvtpuvg"));
}

fn find_password(door_id: &str) -> String {
	let mut chars: Vec<char> = vec![];
	let mut i = 0;

	while chars.len() < 8 {
		let result = next_char(door_id, i);
		chars.push(result.0);
		i = result.1 + 1;
	}

	chars.into_iter().collect()
}

#[test]
fn test_find_password() {
	assert_eq!(find_password("abc"), "18f47a30");
}

fn next_char(door_id: &str, start: i32) -> (char, i32) {
	let mut i = start;

	loop {
		let hash = make_hash(door_id, i);

		if is_valid_hash(&hash) {
			let c = hash.chars().nth(5).unwrap();
			return (c, i);
		}

		i += 1;
	}
}

#[test]
fn test_next_char() {
	assert_eq!(next_char("abc", 0), ('1', 3231929));
	assert_eq!(next_char("abc", 3231930), ('8', 5017308));
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

fn is_valid_hash(hash: &str) -> bool {
	hash.starts_with("00000")
}

#[test]
fn test_is_valid_hash() {
	assert_eq!(is_valid_hash("000001dbbfa"), true);
	assert_eq!(is_valid_hash("00006136ef"), false);
}
