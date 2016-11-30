use std::iter::FromIterator;
use std::io::{self, Read};

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	let packages = input.lines()
		.map(parse_package)
		.map(|o| { o.unwrap() })
		.collect();
	println!("{}", ribbon_needed(packages));
}

fn parse_package(input: &str) -> Option<[i32; 3]> {
	let elems = input.split("x");
	let v = Vec::from_iter(elems);

	if v.len() != 3 {
		return None
	}

	let mut result = [0, 0, 0];

	for i in 0..3 {
		match v[i].parse::<i32>() {
			Ok(n) => result[i] = n,
			Err(_) => return None
		}
	}

	Some(result)
}

#[test]
fn parse_package_success() {
	assert_eq!(parse_package("2x3x4"), Some([2, 3, 4]));
}

#[test]
fn parse_package_not_enough_parts() {
	assert_eq!(parse_package("2x3"), None);
}

fn ribbon_needed(packages: Vec<[i32; 3]>) -> i32 {
	packages.iter().map(|p| {
		smallest_perimeter(p) + bow_length(p)
	}).sum()
}

#[test]
fn test_ribbon_needed() {
	let input = vec![[2, 3, 4]];
	assert_eq!(ribbon_needed(input), 34);
	let inputs = vec![[2, 3, 4], [1, 1, 10]];
	assert_eq!(ribbon_needed(inputs), 48);
}

fn smallest_perimeter(package: &[i32; 3]) -> i32 {
	(0..3).map(|i| {
		let a = package[i];
		let b = package[(i + 1) % 3];
		(a + b) * 2
	}).min().unwrap()
}

#[test]
fn test_smallest_perimeter() {
	assert_eq!(smallest_perimeter(&[4, 3, 2]), 10);
}

fn bow_length(package: &[i32; 3]) -> i32 {
	package[0] * package[1] * package[2]
}

#[test]
fn test_bow_length() {
	assert_eq!(bow_length(&[4, 3, 2]), 24);
}
