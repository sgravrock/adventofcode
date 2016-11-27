use std::iter::FromIterator;

fn main() {
	/*
	let input = vec!["2x3x4"];
	let packages = input.iter().map(parsePackage);
	println!("{}", packages.next()[2]);
	*/
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
