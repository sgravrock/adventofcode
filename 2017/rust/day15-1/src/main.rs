fn main() {
	let mut a = Generator::new(703, 16807);
	let mut b = Generator::new(516, 48271);
	println!("{}\n", count_equal_ish(&mut a, &mut b, 40000000));
}

struct Generator {
	prev_value: u64,
	factor: u64,
}

impl Generator {
	fn new(seed: u64, factor: u64) -> Generator {
		Generator { prev_value: seed, factor: factor }
	}
}

impl Iterator for Generator {
	type Item = u64;

	fn next(&mut self) -> Option<u64> {
		let v = (self.prev_value * self.factor) % 2147483647;
		self.prev_value = v;
		Some(v)
	}
}

#[test]
fn test_generator() {
	let actual_a: Vec<u64> = Generator::new(65, 16807).take(5).collect();
	let actual_b: Vec<u64> = Generator::new(8921, 48271).take(5).collect();
	assert_eq!(actual_a, vec![
		1092455, 
		1181022009,
		245556042,
		1744312007,
		1352636452
	]);
	assert_eq!(actual_b, vec![
		430625591,
		1233683848,
		1431495498,
		137874439,
		285222916
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
fn test_count_equal_ish() {
	let mut a = Generator::new(65, 16807);
	let mut b = Generator::new(8921, 48271);
	assert_eq!(count_equal_ish(&mut a, &mut b, 5), 1);
}
