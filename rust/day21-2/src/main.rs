use std::fmt;

fn main() {
    println!("Hello, world!");
}

#[derive(PartialEq)]
struct Grid {
	lines: Vec<Vec<bool>>
}

impl Grid {
	fn new(lines: Vec<Vec<bool>>) -> Grid {
		Grid { lines: lines }
	}

	fn parse(input: &str) -> Grid {
		let lines = input
			.split("\n")
			.map(|line| line.chars().map(|c| c == '#').collect())
			.collect();
		Grid { lines }
	}

	fn ror(&self) -> Grid {
		if self.lines.len() == 2 {
			Grid::new(vec![
				vec![self.lines[1][0], self.lines[0][0]],
				vec![self.lines[1][1], self.lines[0][1]],
			])
		} else if self.lines.len() == 3 {
			Grid::new(vec![
				vec![self.lines[2][0], self.lines[1][0], self.lines[0][0]],
				vec![self.lines[2][1], self.lines[1][1], self.lines[0][1]],
				vec![self.lines[2][2], self.lines[1][2], self.lines[0][2]],
			])
		} else {
			panic!("Don't know how to transform a grid of size {}",
				self.lines.len());
		}
	}

	fn hflip(&self) -> Grid {
		Grid::new(
			self.lines.iter()
				.map(|line| {
					let mut r = line.clone();
					r.reverse();
					r
				})
				.collect()
		)
	}

	fn vflip(&self) -> Grid {
		let mut lines = self.lines.clone();
		lines.reverse();
		Grid::new(lines)
	}
}

impl fmt::Debug for Grid {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let pretty = self.lines.iter()
			.map(|line| {
				line.iter()
					.map(|v| if *v { '#' } else { '.' })
					.collect::<String>()
			})
			.collect::<Vec<String>>()
			.join("\n");

		writeln!(f, "\n{}", pretty)
	}
}

#[test]
fn test_grid_parse() {
	let expected = Grid::new(vec![vec![false, true], vec![true, false]]);
	let actual = Grid::parse(&strip(
"		.#
		#."));
	assert_eq!(actual, expected)
}

#[test]
fn test_grid_ror_2() {
	let input = Grid::parse(&strip(
"	.#
	##"));
	let expected = Grid::parse(&strip(
"		#.
		##"));
	assert_eq!(input.ror(), expected);
}

#[test]
fn test_grid_ror_3() {
	let input = Grid::parse(&strip(
"	.#.
	##.
	..."));
	let expected = Grid::parse(&strip(
"		.#.
		.##
		..."));
	assert_eq!(input.ror(), expected);
}

#[test]
fn test_grid_hflip() {
	let input = Grid::parse(&strip(
"	..#
	..#
	#.."));
	let expected = Grid::parse(&strip(
"	#..
	#..
	..#"));
	assert_eq!(input.hflip(), expected);
}

#[test]
fn test_grid_vflip() {
	let input = Grid::parse(&strip(
"	..#
	.#.
	#.."));
	let expected = Grid::parse(&strip(
"	#..
	.#.
	..#"));
	assert_eq!(input.vflip(), expected);
}

fn strip(s: &str) -> String {
	s.chars()
		.filter(|c| *c == '.' || *c == '#' || *c == '\n')
		.collect()
}
