fn main() {
}

fn str_to_vec(input: &str) -> Vec<u32> {
	input.chars()
		.map(|c| c.to_digit(10).unwrap())
		.collect()
}

fn fft(input: Vec<u32>, times: u32) -> Vec<u32> {
	(0..times).fold(input, |prev_signal, _| fft_once(prev_signal))
}

fn fft_once(signal: Vec<u32>) -> Vec<u32> {
	let mut result: Vec<u32> = signal.iter().enumerate()
		.map(|(i, _)| {
			let multipliers = fft_multipliers(i, signal.len());
			signal.iter().enumerate()
				.map(|(j, sv)| *sv as i32 * multipliers[j])
				.sum::<i32>()
				.abs() as u32 % 10
		})
		.collect();
	result.resize(8, 0);
	result
}

fn fft_multipliers(digit_ix: usize, len: usize) -> Vec<i32> {
	static BASE_PATTERN: [i32; 4] = [0, 1, 0, -1];

	(0..len)
		.map(|i| {
			let bi = (i + 1) / (digit_ix + 1);
			BASE_PATTERN[bi % 4]
		})
		.collect()
}

#[test]
fn test_fft_short() {
	assert_eq!(fft(str_to_vec("12345678"), 1), str_to_vec("48226158"));
	assert_eq!(fft(str_to_vec("12345678"), 2), str_to_vec("34040438"));
	assert_eq!(fft(str_to_vec("12345678"), 3), str_to_vec("03415518"));
	assert_eq!(fft(str_to_vec("12345678"), 4), str_to_vec("01029498"));
}

#[test]
fn test_fft_long_1() {
	assert_eq!(
		fft(str_to_vec("80871224585914546619083218645595"), 100),
		str_to_vec("24176176")
	);
}

#[test]
fn test_fft_long_2() {
	assert_eq!(
		fft(str_to_vec("19617804207202209144916044189917"), 100),
		str_to_vec("73745418")
	);
}

#[test]
fn test_fft_long_3() {
	assert_eq!(
		fft(str_to_vec("69317163492948606335995924319873"), 100),
		str_to_vec("52432133")
	);
}

#[test]
fn test_fft_multipliers() {
	assert_eq!(fft_multipliers(0, 8), vec![1, 0, -1, 0, 1, 0, -1, 0]);
	assert_eq!(fft_multipliers(1, 8), vec![0, 1, 1, 0, 0, -1, -1, 0]);
}
