mod input;
use std::collections::HashSet;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::f32::consts::PI;
use std::cmp::Ordering;
use std::fmt;
extern crate regex;
use regex::Regex;
#[cfg(test)] use assert_approx_eq::assert_approx_eq;

fn main() {
}

type Coord = (i32, i32);

#[derive(Debug, PartialEq)]
struct Location {
	coord: Coord,
	n_reachable: usize
}

// Represent bearings as integer thousandths of a radian to allow for
// equality comparison and use as hash keys.
#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct Bearing {
	thousandths: i32
}

impl Bearing {
	fn from_radians(radians: f32) -> Bearing {
		Bearing { thousandths: (radians * 1000f32).round() as i32 }
	}
}

impl fmt::Debug for Bearing {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "B:{}", self.thousandths)
	}
}


impl Ord for Bearing {
	fn cmp(&self, other: &Self) -> Ordering {
		self.thousandths.cmp(&other.thousandths)
	}
}

impl PartialOrd for Bearing {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}


struct VaporizationOrder {
	laser_pos: Coord,
	remaining_targets: HashSet<Coord>,
	prev_bearing: Option<Bearing>
}

impl VaporizationOrder {
	fn new(asteroids: &HashSet<Coord>, laser_pos: Coord) -> VaporizationOrder {
		let except_laser = asteroids.iter()
			.cloned()
			.filter(|a| *a != laser_pos)
			.collect();
	
		VaporizationOrder {
			laser_pos,
			remaining_targets: except_laser,
			prev_bearing: None
		}
	}
}

impl Iterator for VaporizationOrder {
	type Item = Coord;

	fn next(&mut self) -> Option<Coord> {
		if self.remaining_targets.len() == 0 {
			return None;
		}

		// TODO: Do this without recomputing all the bearings
		// every time.
		let mut candidates = reachable_asteroids(&self.remaining_targets,
		self.laser_pos);
		assert!(candidates.len() > 0);
		candidates.sort();

		let chosen = match self.prev_bearing {
			None => {
				println!("No previous bearing. Using {:?}", candidates[0]);
				candidates[0]
			},
			Some(prev) => {
				let first_candidate = candidates.iter()
					.cloned()
					.filter(|c| c.0 >= prev)
					.next();

				match first_candidate {
					Some(c) => {
						println!("Prev was {:?}, using candidate {:?}", prev, c);
						c
					},
					None => {
						println!("Prev was {:?}, no match, falling back to {:?}", prev, candidates[0]);
						candidates[0]
					}
				}
			}
		};

		self.prev_bearing = Some(chosen.0);
		self.remaining_targets.remove(&chosen.1);
		Some(chosen.1)
	}
}


fn reachable_asteroids(
	asteroids: &HashSet<Coord>,
	observer: Coord
) -> Vec<(Bearing, Coord)> {
	let mut nearest_by_bearing: HashMap<Bearing, (f32, Coord)> = HashMap::new();

	for a in asteroids {
		if *a != observer {
			let b = bearing(observer, *a);
			let r = range(observer, *a);

			nearest_by_bearing.entry(b)
				.and_modify(|existing| {
					if r < existing.0 {
						*existing = (r, *a);
					}
				})
				.or_insert((r, *a));
		}
	}

	nearest_by_bearing.iter()
		.map(|(b, (_, c))| (*b, *c))
		.collect()
}

fn bearing(src: Coord, dest: Coord) -> Bearing {
	Bearing::from_radians(raw_bearing(src, dest))
}

// The clockwise angle between straight up and the line from src to dest,
// in radians.
fn raw_bearing(src: Coord, dest: Coord) -> f32 {
	let rise = dest.1 - src.1;
	let run = dest.0 - src.0;

	if run == 0 {
		if rise < 0 {
			return 0f32;
		} else if rise > 0 {
			return PI;
		} else {
			panic!("Tried to compute the bearing between identical points {} and {}");
		}
	} else if rise == 0 {
		if run > 0 {
			return PI / 2f32;
		} else {
			return PI / 2f32 * 3f32;
		}
	}

	let quadrant = if run > 0 {
		if rise > 0 {
			0f32
		} else {
			1f32
		}
	} else {
		if rise < 0 {
			2f32
		} else {
			3f32
		}
	};

	let t = (rise as f32 / run as f32).atan();
	t.abs() + PI / 2f32 * quadrant
}

fn range(src: Coord, dest: Coord) -> f32 {
	let rise = dest.1 - src.1;
	let run = dest.0 - src.0;
	((rise.abs() + run.abs()) as f32).sqrt()
}

fn asteroids_from_str(input: &str) -> HashSet<Coord> {
	let whitespace = Regex::new(r"[ \t]+").unwrap();
	let mut result: HashSet<Coord> = HashSet::new();
	whitespace.replace_all(input, "")
		.lines()
		.filter(|line| line.len() != 0)
		.enumerate()
		.for_each(|(y, line)| {
			line.chars()
				.enumerate()
				.for_each(|(x, c)| {
					if c == '#' {
						result.insert((x as i32, y as i32));
					} else if c != '.' {
						panic!("Unexpected input character '{}'", c);
					}
				})
		});

	result
}

#[test]
fn test_vaporization_order() {
	let input = asteroids_from_str("
		.#....#####...#..
		##...##.#####..##
		##...#...#.#####.
		..#.....#...###..
		..#.#.....#....##
	");
	/*
	First 9:
	.#....###24...#..
	##...##.13#67..9#
	##...#...5.8####.
	..#.....X...###..
	..#.#.....#....##

	2nd 9:
	.#....###.....#..
	##...##...#.....#
	##...#......1234.
	..#.....X...5##..
	..#.9.....8....76

	3rd 9:
	.8....###.....#..
	56...9#...#.....#
	34...7...........
	..2.....X....##..
	..1..............
	
	Last three cycles (1-3, 4-8, and 9):
	......234.....6..
	......1...5.....7
	.................
	........X....89..
	.................
	*/
	let laser = (8,3);
	let expected = vec![
		// 1-8
		(8, 1), (9, 0), (9, 1), (10, 0), (9, 2), (10, 1), (11, 1), (11, 2),
		// 9
		(15, 1),
		// 1-8
		(12, 2), (13, 2), (14, 2), (15, 2), (12, 3), (16, 4), (15, 4), (10, 4),
		// 9
		(4, 4),
		// 1-9
		(2, 4), (2, 3), (0, 2), (1, 2), (0, 1), (1, 1), (5, 2), (1, 1), (5, 1),
		// 1-7
		(6, 1), (6, 0), (7, 0), (8, 0), (10, 1), (14, 0), (16, 0),
		(13, 3), (14, 3)
	];
	let actual: Vec<Coord> = VaporizationOrder::new(&input, laser).collect();
	assert_eq!(actual, expected);
}

#[test]
fn test_reachable_asteroids() {
	let input = asteroids_from_str("
		.#..#
		.....
		#####
		....#
		...##
	");

	assert_eq!(reachable_asteroids(&input, (1, 0)).len(), 7);
	assert_eq!(reachable_asteroids(&input, (4, 0)).len(), 7);
	assert_eq!(reachable_asteroids(&input, (0, 2)).len(), 6);
	assert_eq!(reachable_asteroids(&input, (1, 2)).len(), 7);
	assert_eq!(reachable_asteroids(&input, (2, 2)).len(), 7);
	assert_eq!(reachable_asteroids(&input, (3, 2)).len(), 7);
	assert_eq!(reachable_asteroids(&input, (4, 2)).len(), 5);
	assert_eq!(reachable_asteroids(&input, (4, 3)).len(), 7);
	assert_eq!(reachable_asteroids(&input, (3, 4)).len(), 8);
	assert_eq!(reachable_asteroids(&input, (4, 4)).len(), 7);
}


#[test]
fn test_reachable_asteroids_cases_requiring_small_epsilon() {
	let grid = asteroids_from_str("
		.#..##.###...#######
		##.############..##.
		.#.######.########.#
		.###.#######.####.#.
		#####.##.#.##.###.##
		..#####..#.#########
		####################
		#.####....###.#.#.##
		##.#################
		#####.##.###..####..
		..######..##.#######
		####.##.####...##..#
		.#####..#.######.###
		##...#.##########...
		#.##########.#######
		.####.#.###.###.#.##
		....##.##.###..#####
		.#.#.###########.###
		#.#.#.#####.####.###
		###.##.####.##.#..##
	");
	let result: Vec<Coord> = reachable_asteroids(&grid, (11,13))
		.iter()
		.map(|(_, c)| *c)
		.collect();
	assert!(result.contains(&(0,1)));
	assert!(result.contains(&(1,4)));
	assert!(result.contains(&(3,4)));
	assert!(result.contains(&(4,2)));
	assert!(result.contains(&(5,2)));
	assert!(result.contains(&(6,2)));
	assert!(result.contains(&(8,0)));
	assert!(result.contains(&(17,0)));
}

#[test]
fn test_bearing_right_angles() {
	assert_eq!(bearing((0, 0), (0, -1)), Bearing::from_radians(0f32));
	assert_eq!(bearing((0, 0), (1, 0)), Bearing::from_radians(PI / 2f32));
	assert_eq!(bearing((0, 0), (0, 1)), Bearing::from_radians(PI));
	assert_eq!(bearing((0, 0), (-1, 0)), Bearing::from_radians(PI / 2f32 * 3f32));
}

#[test]
fn test_bearing_45deg() {
	assert_eq!(bearing((0, 0), (1, 1)), Bearing::from_radians(PI / 4f32));
	assert_eq!(bearing((0, 0), (1, -1)), Bearing::from_radians(PI / 4f32 * 3f32));
	assert_eq!(bearing((0, 0), (-1, -1)), Bearing::from_radians(PI / 4f32 * 5f32));
	assert_eq!(bearing((0, 0), (-1, 1)), Bearing::from_radians(PI / 4f32 * 7f32));
}

#[test]
fn test_bearing_other() {
	assert_eq!(bearing((0, 0), (2, 1)), Bearing::from_radians(0.4636476));
	assert_eq!(bearing((0, 0), (2, -1)), Bearing::from_radians(2.0344440));
	assert_eq!(bearing((0, 0), (-2, -1)), Bearing::from_radians(3.6052404));
	assert_eq!(bearing((0, 0), (-2, 1)), Bearing::from_radians(5.1760368));
}

#[test]
fn test_asteroids_from_str() {
	let input = "
		.#..#
		.....
		#####
		....#
		...##
	";
	let expected = vec_to_set(vec![
		(1, 0), (4, 0),
		(0, 2), (1, 2), (2, 2), (3, 2), (4, 2),
		(4, 3),
		(3, 4), (4, 4)
	]);
	assert_eq!(asteroids_from_str(input), expected);
}

#[cfg(test)]
fn vec_to_set(vec: Vec<Coord>) -> HashSet<Coord> {
	HashSet::from_iter(vec.iter().cloned())
}

