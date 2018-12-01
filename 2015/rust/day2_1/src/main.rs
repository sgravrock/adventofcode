use std::iter::FromIterator;
use std::io::{self, Read};

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	let packages = input.lines()
		.map(parse_package)
		.map(|o| { o.unwrap() });
	println!("{}", paper_needed(packages));
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

fn paper_needed<TIter: Iterator<Item = [i32; 3]>>(packages: TIter) -> i32 {
	packages.map(|p| {
		surface_area(&p) + slack(&p)
	}).sum()
}

#[test]
fn test_paper_needed() {
	let input = vec![[2, 3, 4]];
	assert_eq!(paper_needed(input.iter().cloned()), 58);
	let inputs = vec![[2, 3, 4], [1, 1, 10]];
	assert_eq!(paper_needed(inputs.iter().cloned()), 58 + 43);
}

fn surface_area(package: &[i32; 3]) -> i32 {
	2 * (face_area(package, 0) + face_area(package, 1) + face_area(package, 2))
}

fn face_area(package: &[i32; 3], face: usize) -> i32 {
	package[face] * package[(face + 1) % 3]
}

#[test]
fn test_surface_area() {
	assert_eq!(surface_area(&[4, 3, 2]), 52);
}

fn slack(package: &[i32; 3]) -> i32 {
	(0..3).map(|i| { face_area(package, i) }).min().unwrap()
}

#[test]
fn test_slack() {
	assert_eq!(slack(&[4, 3, 2]), 6);
}
