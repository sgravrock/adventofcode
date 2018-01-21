extern crate regex;
use regex::Regex;

fn main() {
    println!("Hello, world!");
}

#[derive(Debug)]
#[derive(PartialEq)]
struct Particle {
	id: usize,
	position: (i32, i32, i32),
	velocity: (i32, i32, i32),
	acceleration: (i32, i32, i32),
}

fn parse_input(lines: Vec<&str>) -> Vec<Particle> {
	let re = Regex::new(r"p=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>, v=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>, a=<(-?[\d]+),(-?[\d]+),(-?[\d]+)>").unwrap();

	lines.iter().enumerate()
		.map(|(i, line)| {
			let caps = re.captures(line).unwrap();
			Particle {
				id: i,
				position: (
					caps.get(1).unwrap().as_str().parse().unwrap(),
					caps.get(2).unwrap().as_str().parse().unwrap(),
					caps.get(3).unwrap().as_str().parse().unwrap(),
				),
				velocity: (
					caps.get(4).unwrap().as_str().parse().unwrap(),
					caps.get(5).unwrap().as_str().parse().unwrap(),
					caps.get(6).unwrap().as_str().parse().unwrap(),
				),
				acceleration: (
					caps.get(7).unwrap().as_str().parse().unwrap(),
					caps.get(8).unwrap().as_str().parse().unwrap(),
					caps.get(9).unwrap().as_str().parse().unwrap(),
				),
			}
		})
		.collect()
}

#[test]
fn test_parse_input() {
	let input = vec![
		"p=<-35,0,0>, v=<2,0,0>, a=<-1,0,0>",
		"p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>",
	];
	let expected = vec![
		Particle {
			id: 0, 
			position: (-35, 0, 0),
			velocity: (2, 0, 0),
			acceleration: (-1, 0, 0),
		},
		Particle {
			id: 1, 
			position: (4, 0, 0),
			velocity: (0, 0, 0),
			acceleration: (-2, 0, 0),
		},
	];
	assert_eq!(parse_input(input), expected);
}
