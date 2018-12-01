extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;


fn main() {
    println!("{}", find_password("ojvtpuvg"));
}

#[derive(Debug, PartialEq)]
struct FindCharResult {
	c: char,
	pos: usize,
	hash_num: i32
}

fn find_password(door_id: &str) -> String {
	let mut chars: Vec<char> = vec!['_'; 8];
	let mut i = 0;

	while chars.contains(&'_') {
		let result = next_char(door_id, i).unwrap();

		if chars[result.pos] == '_' {
			chars[result.pos] = result.c;
		}

		i = result.hash_num + 1;
	}

	chars.into_iter().collect()
}

#[test]
fn test_find_password() {
	assert_eq!(find_password("abc"), "05ace8e3");
}

fn next_char(door_id: &str, start: i32) -> Result<FindCharResult, String> {
	let mut i = start;

	loop {
		let hash = make_hash(door_id, i);

		if is_valid_hash(&hash) {
      let hash_s = bytes_to_string(&hash);
			let pos_char = hash_s.chars().nth(5).unwrap();
			match pos_char.to_string().parse::<usize>() {
				Err(_) => {},
				Ok(pos) => {
					if pos < 8 {
						let c = hash_s.chars().nth(6).unwrap();
						return Ok(FindCharResult { c: c, pos: pos, hash_num: i });
					}
				}
			};
		}

		i += 1;
	}
}

#[test]
fn test_next_char() {
	assert_eq!(next_char("abc", 0), Ok(FindCharResult { c: '5', pos: 1, hash_num: 3231929 }));
	assert_eq!(next_char("abc", 3231930), Ok(FindCharResult { c: 'e', pos: 4, hash_num: 5357525 }));
}

fn make_hash(secret: &str, n: i32) -> [u8; 16] {
	let mut digest = Md5::new();
	let input = format!("{}{}", secret, n);
	digest.input_str(&input);
  let mut result = [0u8; 16];
  digest.result(&mut result);
  result
}

fn bytes_to_string(bytes: &[u8]) -> String {
  let strings: Vec<String> = bytes.iter()
    .map(|b| format!("{:02x}", b))
    .collect();
  strings.join("")
}

#[test]
fn test_make_hash() {
	assert_eq!(bytes_to_string(&make_hash("abcdef", 609043)),
    "000001dbbfa3a5c83a2d506429c7b00e");
}

fn is_valid_hash(hash: &[u8; 16]) -> bool {
  hash[0..2] == [0x0, 0x0] && hash[2] < 0x10
}

#[test]
fn test_is_valid_hash() {
		let max_valid = [0x0, 0x0, 0x0f, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0];
		let min_invalid = [0x0, 0x0, 0x10, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0];
		let other_digit = [0x0, 0x01, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0];
  
	assert_eq!(is_valid_hash(&max_valid), true);
	assert_eq!(is_valid_hash(&min_invalid), false);
	assert_eq!(is_valid_hash(&other_digit), false);
}
