fn main() {
    println!("{}", dragon_checksum("10010000000110000", 272));
}

fn dragon(input: &str, len: usize) -> String {
	let mut result: Vec<char> = input.chars().collect();

	while result.len() < len {
		result.push('0');

		for i in (0..(result.len() - 1)).rev() {
			let c = match result[i] {
				'0' => '1',
				'1' => '0',
				_ => result[i]
			};
			result.push(c);
		}
	}

	result.iter().cloned().collect()
}

#[test]
fn test_dragon() {
	assert_eq!("1", dragon("1", 1));
	assert_eq!("100", dragon("1", 3));
	assert_eq!("1111000010100101011110000", dragon("111100001010", 13));
	assert_eq!("10000011110010000111110", dragon("10000", 20));
}

fn checksum(input: &str) -> String {
	let mut s = input.to_string();

	loop {
		s = checksum_once(&s);

		if s.len() % 2 != 0 {
			return s;
		}
	}
}

fn checksum_once(input: &str) -> String {
	let mut result = String::new();
	let chars: Vec<char> = input.chars().collect();
	let mut i = 0;

	while i < chars.len() {
		if i + 1 < chars.len() && chars[i] == chars[i + 1] {
			result.push('1');
		} else {
			result.push('0');
		}

		i += 2;
	}

	result
}

#[test]
fn test_checksum() {
	assert_eq!("100", checksum("110010110100"));
	assert_eq!("01100", checksum("10000011110010000111"));
}

fn dragon_checksum(input: &str, len: usize) -> String {
	let mut d: Vec<char> = dragon(input, len).chars().collect();
	d.truncate(len);
	let limited: String = d.iter().cloned().collect();
	checksum(&limited)
}

#[test]
fn test_dragon_checksum() {
	assert_eq!("01100", dragon_checksum("10000", 20));
}
