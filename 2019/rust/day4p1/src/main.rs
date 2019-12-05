fn main() {
	println!("{}", num_passwords(245182, 790572));
}

fn num_passwords(min: u32, max: u32) -> usize {
	let mut digits = num_to_digits(min);
	let mut n = 0;

	while num_from_digits(&digits) <= max {
		if is_valid_password(&digits) {
			n += 1;
		}

		increment(&mut digits);
	}

	n
}

fn is_valid_password(digits: &Vec<u32>) -> bool {
	let mut has_pair = false;

	for i in 1..digits.len() {
		if digits[i - 1] == digits[i] {
			has_pair = true;
		} else if digits[i - 1] > digits[i] {
			return false;
		}
	}

	has_pair
}

#[test]
fn test_is_valid_password() {
	assert_eq!(is_valid_password(&vec![1,1,1,1,1,1]), true);
	assert_eq!(is_valid_password(&vec![2,2,3,4,5,0]), false);
	assert_eq!(is_valid_password(&vec![1,2,3,7,8,9]), false);
}

fn num_from_digits(digits: &Vec<u32>) -> u32 {
	let mut n: u32 = 0; 

	for i in 0..digits.len() {
		n = n * 10 + digits[i];
	}

	n
}

fn num_to_digits(mut n: u32) -> Vec<u32> {
	let mut digits: Vec<u32> = vec![];

	while n > 0 {
		digits.insert(0, n % 10);
		n /= 10;
	}

	digits
}

fn increment(digits: &mut Vec<u32>) {
	let mut i = digits.len() - 1;

	while i >= 1 && digits[i] == 9 {
		i -= 1;
	}

	let n = digits[i] + 1;

	while i < digits.len() {
		digits[i] = n;
		i += 1;
	}
}

#[test]
fn test_increment() {
	let mut v = vec![1,2,9,9];
	increment(&mut v);
	assert_eq!(v, vec![1,3,3,3]);

	let mut v2 = vec![1,9];
	increment(&mut v2);
	assert_eq!(v2, vec![2,2]);

	let mut v3 = vec![1,2];
	increment(&mut v3);
	assert_eq!(v3, vec![1,3]);
}
