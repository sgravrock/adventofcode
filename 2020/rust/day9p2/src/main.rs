use std::ops::RangeInclusive;
mod input;

fn main() {
	let cipher = input::puzzle_input().lines()
		.map(|s| s.parse().unwrap())
		.collect();
	println!("{:?}", weakness(&cipher, 25));
	 // 438559930
}

fn weakness(cipher: &Vec<u64>, preamble_len: usize) -> Option<u64> {
	first_invalid(cipher, preamble_len)
		.and_then(|invalid_num| range_with_sum(cipher, invalid_num))
		.and_then(|range| {
			let min = range.clone().map(|i| cipher[i]).min().unwrap();
			let max = range.map(|i| cipher[i]).max().unwrap();
			Some(min + max)
		})
}

#[test]
fn test_weakness() {
	let c = vec![35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182,
		127, 219, 299, 277, 309, 576];
	assert_eq!(weakness(&c, 5), Some(62));
}

fn range_with_sum(
	cipher: &Vec<u64>,
	desired_sum: u64
) -> Option<RangeInclusive<usize>> {
	for start in 0..(cipher.len() - 1) { // need at least two
		let mut sum = cipher[start];

		for end in (start + 1)..cipher.len() {
			sum += cipher[end];

			if sum == desired_sum {
				return Some(start..=end);
			}
		}
	}

	None
}

fn first_invalid(cipher: &Vec<u64>, preamble_len: usize) -> Option<u64> {
	(preamble_len..cipher.len())
		.filter(|i| !is_valid(&cipher, *i, preamble_len))
		.map(|i| cipher[i])
		.next()
}

#[test]
fn test_first_invalid() {
	let c = vec![35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182,
		127, 219, 299, 277, 309, 576];
	assert_eq!(first_invalid(&c, 5), Some(127));
}

fn is_valid(cipher: &Vec<u64>, i: usize, preamble_len: usize) -> bool {
	for j in (i - preamble_len)..i {
		for k in (i - preamble_len)..i {
			let a = cipher[j];
			let b = cipher[k];

			if a != b && a + b == cipher[i] {
				return true;
			}
		}
	}

	false
}

#[test]
fn test_is_valid() {
	let mut cipher: Vec<u64> = (1..=26).collect();
	assert_eq!(is_valid(&cipher, 25, 25), true);
	cipher[25] = 49;
	assert_eq!(is_valid(&cipher, 25, 25), true);
	cipher[25] = 100;
	assert_eq!(is_valid(&cipher, 25, 25), false);
	cipher[25] = 50;
	assert_eq!(is_valid(&cipher, 25, 25), false);
}
