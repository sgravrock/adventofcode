fn main() {
	println!("{}", num_passwords(245182, 790572));
}

fn num_passwords(min: u32, max: u32) -> usize {
	(min..(max+1))
		.filter(|&p| is_valid_password(p))
		.count()
}

fn is_valid_password(password: u32) -> bool {
	let digits: Vec<_> = password
		.to_string()
		.chars()
		.map(|c| c.to_digit(10).unwrap())
		.collect();
	let mut has_pair = false;

	for i in 1..digits.len() {
		if digits[i - 1] == digits[i] &&
				(i == 1 || digits[i - 2] != digits[i]) &&
				(i == digits.len() - 1 || digits[i + 1] != digits[i]) {
			has_pair = true;
		}

		if digits[i - 1] > digits[i] {
			return false
		}
	}

	has_pair
}

#[test]
fn test_is_valid_password() {
	assert_eq!(is_valid_password(112233), true);
	assert_eq!(is_valid_password(123444), false);
	assert_eq!(is_valid_password(111122), true);
}
