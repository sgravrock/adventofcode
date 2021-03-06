use std::io::{self, Read};

fn main() {
	let mut input = String::new();
	io::stdin().read_to_string(&mut input)
		.expect("Failed to read input");
	let candidates = parse_input(&input);

	let triangles: Vec<&[i32;3]> = candidates.iter()
		.filter(|c| is_triangle(**c))
		.collect();
	println!("{}", triangles.len());
}

fn parse_input(input: &str) -> Vec<[i32;3]> {
	input.split("\n")
		.map(|line| {
			let v: Vec<i32> = line.split_whitespace()
				.map(|token| token.parse::<i32>().unwrap())
				.collect();
			[v[0], v[1], v[2]]
		})
		.collect()
}

#[test]
fn test_parse_input() {
  let input = "  810  679   10\n 783  255  616";
  let result = parse_input(input);
  assert_eq!(vec![[810, 679, 10], [783, 255, 616]], result);
}

fn is_triangle(sides: [i32;3]) -> bool {
	sides[0] + sides[1] > sides[2] &&
		sides[1] + sides[2] > sides[0] &&
		sides[0] + sides[2] > sides[1]
}

#[test]
fn test_is_triangle_no() {
	assert_eq!(false, is_triangle([5, 10, 25]));
	assert_eq!(false, is_triangle([10, 25, 5]));
	assert_eq!(false, is_triangle([25, 5, 10]));
}

#[test]
fn test_is_triangle_yes() {
	assert_eq!(true, is_triangle([2, 3, 4]));
	assert_eq!(true, is_triangle([3, 4, 2]));
	assert_eq!(true, is_triangle([3, 2, 4]));
}
