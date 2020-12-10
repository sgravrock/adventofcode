mod input;

fn main() {
	let cipher = input::puzzle_input().lines()
		.map(|s| s.parse().unwrap())
		.collect();
    println!("{:?}", solve(cipher, 25));
	 // 3199139634
}

fn solve(cipher: Vec<u64>, preamble_len: usize) -> Option<u64> {
	(preamble_len..cipher.len())
		.filter(|i| !is_valid(&cipher, *i, preamble_len))
		.map(|i| cipher[i])
		.next()
}

#[test]
fn test_solve() {
	let c = vec![35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182,
		127, 219, 299, 277, 309, 576];
	assert_eq!(solve(c, 5), Some(127));
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
