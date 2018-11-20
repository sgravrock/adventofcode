use std::fmt;

fn main() {
	let input = 
"../.. => .#./.../###
#./.. => .#./##./#..
##/.. => #.#/#../###
.#/#. => ##./..#/###
##/#. => .#./#../..#
##/## => #../..#/#.#
.../.../... => .###/.#.#/.###/##.#
#../.../... => .##./##../##../#.##
.#./.../... => .#.#/#.#./..#./..#.
##./.../... => ###./#.##/...#/#.##
#.#/.../... => .#.#/.#../.###/.###
###/.../... => ..##/#.#./..../##.#
.#./#../... => #.../..../..../....
##./#../... => ...#/..#./.###/#.#.
..#/#../... => #.../#.##/###./##..
#.#/#../... => .##./#..#/#..#/..##
.##/#../... => .#.#/#.##/..##/.#.#
###/#../... => #.#./.###/..#./#.#.
.../.#./... => #..#/..../.##./.#.#
#../.#./... => .#../.##./.#.#/...#
.#./.#./... => ##.#/...#/.##./...#
##./.#./... => ..#./#.#./#.##/####
#.#/.#./... => ..##/#..#/.###/....
###/.#./... => .#../#..#/#.../..#.
.#./##./... => ..##/#.#./####/###.
##./##./... => ...#/.#../####/#..#
..#/##./... => ..##/##../###./....
#.#/##./... => ..##/#.../.#../.##.
.##/##./... => #.../##../#.##/...#
###/##./... => .#../####/#.##/#.##
.../#.#/... => #..#/####/###./#.#.
#../#.#/... => #.../##.#/#.../.#..
.#./#.#/... => ##.#/##.#/..#./..#.
##./#.#/... => .###/..#./.#../.###
#.#/#.#/... => .###/##../..#./..#.
###/#.#/... => ##../.#../.#../.#..
.../###/... => ..#./#.#./..#./#..#
#../###/... => ..../#.#./##.#/..##
.#./###/... => ..#./#.#./..##/.#..
##./###/... => .##./..##/#..#/#.#.
#.#/###/... => ###./###./#.##/..##
###/###/... => ##.#/..../.##./.#..
..#/.../#.. => .###/####/..../##.#
#.#/.../#.. => ##../###./#..#/...#
.##/.../#.. => ###./#..#/###./...#
###/.../#.. => #.../#..#/##.#/.##.
.##/#../#.. => ..##/####/..##/#...
###/#../#.. => #.../..../...#/..##
..#/.#./#.. => ####/#.#./..../.#.#
#.#/.#./#.. => .##./.#.#/##.#/.##.
.##/.#./#.. => ###./.#.#/###./##.#
###/.#./#.. => #.##/..##/#.#./##.#
.##/##./#.. => ..../..##/#.#./.##.
###/##./#.. => #.#./#..#/#..#/###.
#../..#/#.. => ..../####/#..#/.###
.#./..#/#.. => .###/#.../#.../#.##
##./..#/#.. => ####/##.#/###./####
#.#/..#/#.. => .#../##.#/#..#/#..#
.##/..#/#.. => ..##/##.#/#.##/###.
###/..#/#.. => ##.#/####/##.#/.#..
#../#.#/#.. => .###/#..#/.##./.###
.#./#.#/#.. => #.##/.##./.#../..#.
##./#.#/#.. => ###./..#./.##./##..
..#/#.#/#.. => .###/.#.#/#.#./##..
#.#/#.#/#.. => #..#/.###/.##./....
.##/#.#/#.. => ###./.###/#.##/.###
###/#.#/#.. => ####/.###/..../.##.
#../.##/#.. => ##.#/..../#.../..#.
.#./.##/#.. => #.../..../...#/###.
##./.##/#.. => ###./.#../..##/...#
#.#/.##/#.. => #.../...#/..#./.###
.##/.##/#.. => ###./..../##.#/...#
###/.##/#.. => ##.#/##../###./.##.
#../###/#.. => ..#./#.../..##/#.##
.#./###/#.. => ...#/.##./.#../.#..
##./###/#.. => ##.#/.#.#/###./....
..#/###/#.. => #.##/#.../####/.##.
#.#/###/#.. => .#.#/...#/#..#/..#.
.##/###/#.. => .##./#..#/#..#/.#.#
###/###/#.. => ###./####/#.##/#...
.#./#.#/.#. => ###./#..#/...#/...#
##./#.#/.#. => #.#./#.##/#.../#..#
#.#/#.#/.#. => .#.#/#.##/..../.#..
###/#.#/.#. => #.#./.#../.###/#.#.
.#./###/.#. => #.../.###/##../##.#
##./###/.#. => .###/#.../####/.#.#
#.#/###/.#. => #..#/####/#.#./#...
###/###/.#. => .#../..../.##./.#.#
#.#/..#/##. => ##../###./...#/###.
###/..#/##. => .##./###./.###/#.##
.##/#.#/##. => ..../##.#/#..#/#...
###/#.#/##. => .###/##../..../..#.
#.#/.##/##. => ####/.###/##../...#
###/.##/##. => #.##/..##/..#./#..#
.##/###/##. => ..../#.##/#.../#.##
###/###/##. => ..../#..#/#.##/#.##
#.#/.../#.# => #.../##.#/..../.#.#
###/.../#.# => ##../##../#.#./.##.
###/#../#.# => .##./.#../#.##/.##.
#.#/.#./#.# => #.../.#../####/#.##
###/.#./#.# => .###/##.#/#.../#.#.
###/##./#.# => .##./.##./.###/.#.#
#.#/#.#/#.# => ####/####/###./.##.
###/#.#/#.# => #.#./.###/...#/.#.#
#.#/###/#.# => .###/..#./..../.##.
###/###/#.# => #.#./##.#/..#./..#.
###/#.#/### => ###./#.../##../##..
###/###/### => ##.#/.#.#/#.#./...#";
	let rules = Rule::parse(input);
	println!("Puzzle solution is {}.", pixels_on_after_iterations(&rules, 18));
}

fn pixels_on_after_iterations(rules: &Vec<Rule>, iters: usize) -> usize {
	let initial_grid = Grid::default();
	let final_grid = (0..iters)
		.fold(initial_grid, |grid, _| iterate(&grid, rules));
	final_grid.pixels_on()
}

#[test]
fn test_pixels_on_after_iterations_example() {
	let rules = Rule::parse("../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#");
	assert_eq!(pixels_on_after_iterations(&rules, 2), 12);
}

fn iterate(grid: &Grid, rules: &Vec<Rule>) -> Grid {
	let new_blocks: Vec<Vec<Grid>> = grid.split()
		.lines
		.iter()
		.map(|row_of_blocks| {
			row_of_blocks.iter()
				.map(|block| {
					match rules.iter().find(|r| r.matches(block)) {
						None => panic!("No matching rule for block: {:?}", block),
						Some(rule) => rule.output.clone()
					}
				})
				.collect()
		})
		.collect();

	GridOfGrids::new(new_blocks).join()
}

#[test]
fn test_iterate_3x3() {
	let rules = Rule::parse("../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#");
	let expected = Grid::parse(&strip(
"		#..#
		....
		....
		#..#"));
	let actual = iterate(&Grid::default(), &rules);
	assert_eq!(actual, expected);
}

#[test]
fn test_iterate_4x4() {
	let rules = Rule::parse("../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#");
	let input_grid = Grid::parse(&strip(
"		#..#
		....
		....
		#..#"));
	let expected = Grid::parse(&strip(
"		##.##.
		#..#..
		......
		##.##.
		#..#..
		......"));
	let actual = iterate(&input_grid, &rules);
	assert_eq!(actual, expected);
}

#[test]
fn test_iterate_example() {
	let rules = Rule::parse("../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#");
	let expected = Grid::parse(&strip(
"		##.##.
		#..#..
		......
		##.##.
		#..#..
		......"));
	let actual = iterate(&iterate(&Grid::default(), &rules), &rules);
	assert_eq!(actual, expected);
}

#[derive(PartialEq, Clone)]
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

	fn default() -> Grid {
		Grid::parse(&strip(
"			.#.
			..#
			###"))
	}

	fn pixels_on(&self) -> usize {
		let mut n = 0;

		for line in &self.lines {
			n += line.iter().filter(|x| **x).count();
		}

		n
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

	fn split(&self) -> GridOfGrids {
		if self.lines.len() % 2 == 0 {
			self.split_n(2)
		} else if self.lines.len() % 3 == 0 {
			self.split_n(3)
		} else {
			panic!("Grid size {} is not divisible by 2 or 3", self.lines.len());
		}
	}

	fn split_n(&self, blocksize: usize) -> GridOfGrids {
		let blocks_per_side = self.lines.len() / blocksize;
		GridOfGrids::new(
			(0..blocks_per_side)
				.map(|y| {
					(0..blocks_per_side)
						.map(|x| self.extract_block(y, x, blocksize))
						.collect()
				})
				.collect()
		)
	}

	fn extract_block(&self, by: usize, bx: usize, blocksize: usize) -> Grid {
		let lines = (0..blocksize)
			.map(|y| {
				(0..blocksize)
					.map(|x| self.lines[by * blocksize + y][bx * blocksize + x])
					.collect()
			})
			.collect();
		Grid::new(lines)
	}

	fn fmt_row(&self, y: usize) -> String {
		self.lines[y].iter()
			.map(|v| if *v { '#' } else { '.' })
			.collect()
	}
}

impl fmt::Debug for Grid {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut result = writeln!(f, "");

		for y in 0..self.lines.len() {
			result = result.and_then(|_| writeln!(f, "{}", self.fmt_row(y)));
		}

		result
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

#[test]
fn test_grid_extract_block_2() {
	let input = Grid::parse(&strip(
"	#..#
	....
	....
	#..#"));
	let expected = Grid::parse(&strip(
"	..
	#."));
	assert_eq!(input.extract_block(1, 0, 2), expected);
}

#[test]
fn test_grid_split_2() {
	let input = Grid::parse(&strip(
"	#..#
	....
	....
	#..#"));
	let expected = GridOfGrids::new(vec![
		vec![
			Grid::parse(&strip(
"				#.
				..")),
			Grid::parse(&strip(
"				.#
				..")),
		],
		vec![
			Grid::parse(&strip(
"				..
				#.")),
			Grid::parse(&strip(
"				..
				.#")),
		],
	]);
	assert_eq!(input.split(), expected);
}

#[test]
fn test_grid_split_join_2_identity() {
	let input = Grid::parse(&strip(
"	#..#
	....
	....
	#..#"));
	assert_eq!(input.split().join(), input);
}

#[test]
fn test_grid_split_3() {
	let input = Grid::parse(&strip(
"		##.#.#...
		#..#.....
		.........
		##.##....
		#..#.....
		.#...#...
		.........
		.........
		........."));
	let expected = GridOfGrids::new(vec![
		vec![
			Grid::parse(&strip(
"				##.
				#..
				...")),
			Grid::parse(&strip(
"				#.#
				#..
				...")),
			Grid::parse(&strip(
"				...
				...
				...")),
		],
		vec![
			Grid::parse(&strip(
"				##.
				#..
				.#.")),
			Grid::parse(&strip(
"				##.
				#..
				..#")),
			Grid::parse(&strip(
"				...
				...
				...")),
		],
		vec![
			Grid::parse(&strip(
"				...
				...
				...")),
			Grid::parse(&strip(
"				...
				...
				...")),
			Grid::parse(&strip(
"				...
				...
				...")),
		]
	]);
	assert_eq!(input.split(), expected);
}

#[test]
fn test_grid_split_join_3_identity() {
	let input = Grid::parse(&strip(
"		##.#.#...
		#..#.....
		.........
		##.##....
		#..#.....
		.#...#...
		.........
		.........
		........."));
	assert_eq!(input.split().join(), input);
}


#[derive(PartialEq)]
struct GridOfGrids {
	lines: Vec<Vec<Grid>>
}

impl GridOfGrids {
	fn new(lines: Vec<Vec<Grid>>) -> GridOfGrids {
		GridOfGrids { lines: lines }
	}

	fn join(&self) -> Grid {
		let input_gs = self.lines[0][0].lines.len();
		let output_gs = self.lines.len() * input_gs;

		Grid::new(
			(0..output_gs)
				.map(|y| {
					(0..output_gs)
						.map(|x| {
							let grid = &self.lines[y / input_gs][x / input_gs];
							grid.lines[y % input_gs][x % input_gs]
						})
						.collect()
				})
				.collect()
		)
	}

	fn fmt_row(&self, f: &mut fmt::Formatter, gy: usize) -> fmt::Result {
		let ngrids = self.lines.len();
		let cells_per_grid = self.lines[0][0].lines.len();
		let mut result: fmt::Result = Ok(());

		for ly in 0..cells_per_grid {
			for gx in 0..ngrids {
				let grid = &self.lines[gy][gx];
				let subline = grid.fmt_row(ly);
				result = result.and_then(|_| write!(f, "{}", subline));

				if gx < ngrids - 1 {
					result = result.and_then(|_| write!(f, "|"));
				}
			}

			result = result.and_then(|_| writeln!(f, ""));
		}

		result
	}

	fn fmt_hline(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let ngrids = self.lines.len();
		let cells_per_grid = self.lines[0][0].lines.len();
		let nchars = ngrids * cells_per_grid + ngrids - 1;
		let mut result: fmt::Result = Ok(());

		for _ in 0..nchars {
			result = result.and_then(|_| write!(f, "-"));
		}

		result.and_then(|_| writeln!(f, ""))
	}
}

impl fmt::Debug for GridOfGrids {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let mut result = writeln!(f, "");

		for y in 0..self.lines.len() {
			result = result.and_then(|_| self.fmt_row(f, y));

			if y < self.lines.len() - 1 {
				result = result.and_then(|_| self.fmt_hline(f));
			}
		}

		result
	}
}

#[test]
fn test_grid_of_grids_join() {
	let input = GridOfGrids::new(vec![
		vec![
			Grid::parse(&strip(
"				#.
				..")),
			Grid::parse(&strip(
"				.#
				..")),
		],
		vec![
			Grid::parse(&strip(
"				..
				#.")),
			Grid::parse(&strip(
"				..
				.#")),
		],
	]);
	let expected = Grid::parse(&strip(
"	#..#
	....
	....
	#..#"));
	assert_eq!(input.join(), expected);
}


#[derive(PartialEq, Debug)]
struct Rule {
	input: Grid,
	output: Grid,
}

impl Rule {
	fn new(input: Grid, output: Grid) -> Rule {
		Rule { input: input, output: output }
	}

	fn parse(input: &str) -> Vec<Rule> {
		input.split("\n")
			.map(Rule::parse_one)
			.collect()
	}

	fn parse_one(input: &str) -> Rule {
		let in_out = input.split(" => ").collect::<Vec<&str>>();
		Rule::new(
			Rule::parse_grid(in_out[0]),
			Rule::parse_grid(in_out[1])
		)
	}

	fn parse_grid(input: &str) -> Grid {
		Grid::parse(&input.replace("/", "\n"))
	}

	fn matches(&self, grid: &Grid) -> bool {
		grid == &self.input ||
			grid.hflip() == self.input ||
			grid.vflip() == self.input ||
			self.matches_with_rotation(grid) ||
			self.matches_with_rotation(&grid.vflip())
	}

	fn matches_with_rotation(&self, grid: &Grid) -> bool {
		grid.ror() == self.input ||
			grid.ror().ror() == self.input ||
			grid.ror().ror().ror() == self.input
	}
}

#[test]
fn test_rule_parse() {
	let input = "../.. => .#./.../###\n#./.. => .#./##./#..";
	let expected = vec![
		Rule::new(
			Grid::parse(&strip(
"				..
				..")),
			Grid::parse(&strip(
"				.#.
				...
				###"))
		),
		Rule::new(
			Grid::parse(&strip(
"				#.
				..")),
			Grid::parse(&strip(
"				.#.
				##.
				#.."))
		)
	];
	assert_eq!(Rule::parse(input), expected);
}

#[test]
fn test_rule_matches_exact() {
	let grid = Grid::parse(&strip(
"		#.
		.#"));
	let rule = Rule::new(grid.clone(), Grid::new(vec![]));
	assert!(rule.matches(&grid));
}

#[test]
fn test_rule_matches_hflip() {
	let input = Grid::parse(&strip(
"		.#
		#."));
	let grid = Grid::parse(&strip(
"		#.
		.#"));
	let rule = Rule::new(input, Grid::new(vec![]));
	assert!(rule.matches(&grid));
}

#[test]
fn test_rule_matches_vflip() {
	let input = Grid::parse(&strip(
"		..
		##"));
	let grid = Grid::parse(&strip(
"		##
		.."));
	let rule = Rule::new(input, Grid::new(vec![]));
	assert!(rule.matches(&grid));
}

#[test]
fn test_rule_matches_ror_3times() {
	let input = Grid::parse(&strip(
"		.#.
		..#
		###"));
	let grid = Grid::parse(&strip(
"		#..
		#.#
		##."));
	let rule = Rule::new(input, Grid::new(vec![]));
	assert!(rule.matches(&grid));
}

#[test]
fn test_rule_matches_vflip_plus_rotate() {
	let input = Grid::parse(&strip(
"		..#
		#.#
		.##"));
	let grid = Grid::parse(&strip(
"		.#.
		..#
		###"));
	let rule = Rule::new(input, Grid::new(vec![]));
	assert!(rule.matches(&grid));
}

fn strip(s: &str) -> String {
	s.chars()
		.filter(|c| *c == '.' || *c == '#' || *c == '\n')
		.collect()
}
