fn main() {
    println!("{}", dragon_checksum("10010000000110000", 272));
}

fn dragon(input: &str, len: usize) -> Vec<char> {
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

	result
}

#[test]
fn test_dragon() {
	let r1: String = dragon("1", 1).iter().cloned().collect();
	assert_eq!("1", r1);
	let r2: String = dragon("1", 3).iter().cloned().collect();
	assert_eq!("100", r2);
	let r3: String = dragon("111100001010", 13).iter().cloned().collect();
	assert_eq!("1111000010100101011110000", r3);
	let r4: String = dragon("10000", 20).iter().cloned().collect();
	assert_eq!("10000011110010000111110", r4);
}

fn to_checksum(chars: &mut Vec<char>) {
	while chars.len() % 2 == 0 {
		to_checksum_once(chars);
	}
}

fn to_checksum_once(chars: &mut Vec<char>) {
	assert!(chars.len() % 2 == 0);
	let mut i = 0;
	let initial_len = chars.len();

	while i < initial_len / 2 {
		chars[i] = if chars[i*2] == chars[i*2 + 1] {
			'1'
		} else {
			'0'
		};

		i += 1;
	}

	chars.truncate(initial_len / 2);
}

#[test]
fn test_checksum() {
	let mut v1: Vec<char> = "110010110100".chars().collect();
	to_checksum(&mut v1);
	let r1: String = v1.iter().cloned().collect();
	assert_eq!("100", r1);

	let mut v2: Vec<char> = "10000011110010000111".chars().collect();
	to_checksum(&mut v2);
	let r2: String = v2.iter().cloned().collect();
	assert_eq!("01100", r2);
}

fn dragon_checksum(input: &str, len: usize) -> String {
	let mut d = dragon(input, len);
	d.truncate(len);
	to_checksum(&mut d);
	d.iter().cloned().collect()
}

#[test]
fn test_dragon_checksum() {
	assert_eq!("01100", dragon_checksum("10000", 20));
}
