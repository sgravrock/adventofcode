use std::iter::FromIterator;
use std::num::ParseIntError;

fn main() {
	/*
	let input = vec!["2x3x4"];
	let packages = input.iter().map(parsePackage);
	println!("{}", packages.next()[2]);
	*/
}

fn parse_package(input: &str) -> Option<[i32; 3]> {
	let elems = input.split("x")
		.map(|s| s.parse::<i32>())
		.filter(is_ok);
	let v = Vec::from_iter(elems);

	match v.len() {
		// TODO: Is there a more idiomatic way to convert a vector of known length
		// to an array?
		3 => unwrap_each([&v[0], &v[1], &v[2]]),
		_ => None
	}
}

fn is_ok<Tv, Te>(result: &Result<Tv, Te>) -> bool {
	match *result {
		Ok(_) => true,
		Err(_) => false,
	}
}


fn unwrap_each(input: [&Result<i32, ParseIntError>; 3]) -> Option<[i32; 3]> {
	match *input[0] {
		Ok(a) => match *input[1] {
			Ok(b) => match *input[2] {
				Ok(c) => Some([a, b, c]),
				Err(_) => None
			},
			Err(_) => None
		},
		Err(_) => None
	}
}

#[test]
fn parse_package_success() {
	assert_eq!(parse_package("2x3x4"), Some([2, 3, 4]));
}

#[test]
fn parse_package_not_enough_parts() {
	assert_eq!(parse_package("2x3"), None);
}

#[test]
fn unwrap_each_all_some() {
	let input = [&Ok(1), &Ok(2), &Ok(3)];
	assert_eq!(unwrap_each(input), Some([1, 2, 3]));
}

#[test]
fn unwrap_each_not_all_none() {
	let input = [&Ok(1), &Ok(2), &"bogus".parse::<i32>()];
	assert_eq!(unwrap_each(input), None);
}
