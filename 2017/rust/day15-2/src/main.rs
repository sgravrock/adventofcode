fn main() {
	let mut a = Generator::new(703, 16807, 4);
	let mut b = Generator::new(516, 48271, 8);
	println!("{}\n", count_equal_ish(&mut a, &mut b, 5000000));
}

struct Generator {
	prev_value: u64,
	factor: u64,
	criterion: u64,
}

impl Generator {
	fn new(seed: u64, factor: u64, criterion: u64) -> Generator {
		Generator { prev_value: seed, factor: factor, criterion: criterion }
	}
}

impl Iterator for Generator {
	type Item = u64;

	fn next(&mut self) -> Option<u64> {
		let v = (self.prev_value * self.factor) % 2147483647;
		self.prev_value = v;

		if v % self.criterion == 0 {
			Some(v)
		} else {
			self.next()
		}
	}
}

#[test]
fn test_generator() {
	let actual_a: Vec<u64> = Generator::new(65, 16807, 4).take(5).collect();
	let actual_b: Vec<u64> = Generator::new(8921, 48271, 8).take(5).collect();
	assert_eq!(actual_a, vec![
		1352636452,
		1992081072,
		530830436,
		1980017072,
		740335192,
	]);
	assert_eq!(actual_b, vec![
		1233683848,
		862516352,
		1159784568,
		1616057672,
		412269392,
	]);
}

fn equal_ish(a: u64, b:u64) -> bool {
	a & 0xFFFF == b & 0xFFFF
}

#[test]
fn test_equal_ish() {
	assert!(!equal_ish(1092455, 430625591));
	assert!(equal_ish(245556042, 1431495498));
}

fn count_equal_ish(a: &mut Generator, b: &mut Generator, n_iters: usize) -> usize {
	a.take(n_iters)
		.zip(b.take(n_iters))
		.filter(|&(x, y)| equal_ish(x, y))
		.count()
}

#[test]
fn test_count_equal_ish_short() {
	let mut a = Generator::new(65, 16807, 4);
	let mut b = Generator::new(8921, 48271, 8);
	assert_eq!(count_equal_ish(&mut a, &mut b, 1056), 1);
}

#[test]
fn test_count_equal_ish_long() {
	let mut a = Generator::new(65, 16807, 4);
	let mut b = Generator::new(8921, 48271, 8);
	assert_eq!(count_equal_ish(&mut a, &mut b, 5000000), 309);
}
