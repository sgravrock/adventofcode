extern crate crypto;
use crypto::md5::Md5;
use crypto::digest::Digest;


fn main() {
    println!("{}", find_password("ojvtpuvg"));
}

fn find_password(door_id: &str) -> String {
  let pwd = Password::new(door_id);
	pwd.collect::<String>()
}

#[test]
fn test_find_password() {
	assert_eq!(find_password("abc"), "18f47a30");
}

struct Password<'a> {
	door_id: &'a str,
	next_hash_suffix: i32,
	next_pos: i32,
}

impl <'a> Password<'a> {
	fn new(door_id: &'a str) -> Password {
		Password {
			door_id: door_id,
			next_hash_suffix: 0,
			next_pos: 0,
		}
	}
}

impl <'a> Iterator for Password<'a> {
	type Item = char;

	fn next(&mut self) -> Option<char> {
		match self.next_pos {
			0 ... 7 => {
				let r = next_char(&self.door_id, self.next_hash_suffix);
				self.next_hash_suffix = r.1 + 1;
				self.next_pos += 1;
				Some(r.0)
			},
			_ => None
		}
	}
}

fn next_char(door_id: &str, start: i32) -> (char, i32) {
	let mut i = start;

	loop {
		let hash = make_hash(door_id, i);

		if is_valid_hash(&hash) {
			let c = bytes_to_string(&hash).chars().nth(5).unwrap();
			return (c, i);
		}

		i += 1;
	}
}

#[test]
fn test_next_char() {
	let mut pwd = Password::new("abc");
	assert_eq!(pwd.next(), Some('1'));
	assert_eq!(pwd.next_hash_suffix, 3231930);
	assert_eq!(pwd.next(), Some('8'));
	assert_eq!(pwd.next_hash_suffix, 5017309);
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
