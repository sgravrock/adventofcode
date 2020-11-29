mod input;
use std::convert::TryInto;

fn main() {
	println!("{:?}", fft_first_8(str_to_vec(input::puzzle_input()), 100));
	// 30550349
}

fn str_to_vec(input: &str) -> Vec<u32> {
	input.chars()
		.map(|c| c.to_digit(10).unwrap())
		.collect()
}

fn fft_first_8(input: Vec<u32>, times: u32) -> Vec<u32> {
	let mut result = fft(input, times);
	result.truncate(8);
	result
}

#[test]
fn test_fft_first_8() {
	assert_eq!(
		fft_first_8(str_to_vec("80871224585914546619083218645595"), 100),
		str_to_vec("24176176")
	);
	assert_eq!(
		fft_first_8(str_to_vec("19617804207202209144916044189917"), 100),
		str_to_vec("73745418")
	);
	assert_eq!(
		fft_first_8(str_to_vec("69317163492948606335995924319873"), 100),
		str_to_vec("52432133")
	);
}

fn fft(input: Vec<u32>, times: u32) -> Vec<u32> {
	let patterns: Vec<Vec<i32>> = (0..input.len())
		.map(|i| repeated_pattern(i, input.len()))
		.collect();
	fft_cached(input, times, &patterns)
}

fn fft_cached(
	input: Vec<u32>,
	times: u32,
	patterns: &Vec<Vec<i32>>
) -> Vec<u32> {
	if times == 0 {
		return input;
	}

	let mut next_state = Vec::with_capacity(input.len());

	for i in 0..input.len() {
		next_state.push(fft_digit(&input, i, &patterns));
	}

	fft_cached(next_state, times - 1, &patterns)
}

fn fft_digit(
	input: &Vec<u32>,
	output_pos: usize,
	patterns: &Vec<Vec<i32>>
) -> u32 {
	let mut total: i32 = 0;

	for i in 0..input.len() {
		let v: i32 = input[i].try_into().unwrap();
		total += v * patterns[output_pos][i];
	}

	(total % 10).abs().try_into().unwrap()
}

#[test]
fn test_fft() {
	let input = str_to_vec("12345678");
	assert_eq!(fft(input.clone(), 1), str_to_vec("48226158"));
	assert_eq!(fft(input.clone(), 2), str_to_vec("34040438"));
	assert_eq!(fft(input.clone(), 3), str_to_vec("03415518"));
	assert_eq!(fft(input.clone(), 4), str_to_vec("01029498"));
}

fn pattern(output_pos: usize) -> Vec<i32> {
	let base = &[0, 1, 0, -1];
	let mut result = Vec::with_capacity(
		(4 * output_pos).try_into().unwrap()
	);

	for x in base {
		for _ in 0..(output_pos + 1) {
			result.push(*x);
		}
	}

	result
}

#[test]
fn test_pattern() {
	assert_eq!(pattern(0), vec![0, 1, 0, -1]);
	assert_eq!(pattern(2), vec![0, 0, 0, 1, 1, 1, 0, 0, 0, -1, -1, -1]);
}

fn repeated_pattern(output_pos: usize, len: usize) -> Vec<i32> {
	let to_repeat = pattern(output_pos);
	let mut result = Vec::with_capacity(len);

	for i in 0..len {
		result.push(to_repeat[(i + 1) % to_repeat.len()]);
	}

	result
}

#[test]
fn test_repeated_pattern() {
	assert_eq!(
		repeated_pattern(1, 18),
		vec![0, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, -1, -1, 0, 0, 1]
	);
}
