fn main() {
	println!("{}", num_passwords(245182, 790572));
}

fn num_passwords(min: u32, max: u32) -> usize {
	(min..(max+1))
		.filter(is_valid_password)
		.count()
}

fn is_valid_password(password: &u32) -> bool {
	let digits: Vec<_> = password
		.to_string()
		.chars()
		.map(|c| c.to_digit(10).unwrap())
		.collect();
	let mut has_pair = false;

	for i in 1..digits.len() {
		if digits[i - 1] == digits[i] {
			has_pair = true
		} else if digits[i - 1] > digits[i] {
			return false
		}
	}

	has_pair
}
