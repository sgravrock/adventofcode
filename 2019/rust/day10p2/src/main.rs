mod input;
use std::collections::HashSet;
use std::collections::HashMap;
use std::iter::FromIterator;
use std::f32::consts::PI;
extern crate regex;
use regex::Regex;
#[cfg(test)] use assert_approx_eq::assert_approx_eq;

fn main() {
	let result = best_location(&asteroids_from_str(input::puzzle_input()));
	println!("{:?}", result);
}

type Coord = (i32, i32);

#[derive(Debug, PartialEq)]
struct Location {
	coord: Coord,
	n_reachable: usize
}

fn best_location(asteroids: &HashSet<Coord>) -> Location {
	asteroids.iter()
		.map(|observer| {
			let reachable = reachable_asteroids(asteroids, *observer);
			Location { coord: *observer, n_reachable: reachable.len() }
		})
		.max_by_key(|loc| loc.n_reachable)
		.unwrap()
}

fn reachable_asteroids(
	asteroids: &HashSet<Coord>,
	observer: Coord
) -> HashSet<Coord> {
	// Map from bearing in integer thousandths of radians to a tuple of
	// distance to nearest so far and nearest so far.
	// (Rust wisely won't let us use floats as hash keys.)
	fn key_for_bearing(b: f32) -> i32 {
		(b * 1000f32).round() as i32
	}
	let mut nearest_by_bearing: HashMap<i32, (f32, Coord)> = HashMap::new();

	for a in asteroids {
		if *a != observer {
			let b = bearing(observer, *a);
			let r = range(observer, *a);

			nearest_by_bearing.entry(key_for_bearing(b))
				.and_modify(|existing| {
					if r < existing.0 {
						*existing = (r, *a);
					}
				})
				.or_insert((r, *a));
		}
	}

	HashSet::from_iter(nearest_by_bearing.values().map(|(_, a)| a).cloned())
}

// The clockwise angle between straight up and the line from src to dest,
// in radians.
fn bearing(src: Coord, dest: Coord) -> f32 {
	let rise = dest.1 - src.1;
	let run = dest.0 - src.0;

	if run == 0 {
		if rise > 0 {
			return 0f32;
		} else if rise < 0 {
			return PI;
		} else {
			panic!("Tried to compute the bearing between identical points");
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
fn test_best_location() {
	let grid1 = asteroids_from_str("
		.#..#
		.....
		#####
		....#
		...##
	");
	assert_eq!(best_location(&grid1), Location {coord: (3, 4), n_reachable: 8});

	let grid2 = asteroids_from_str("
		......#.#.
		#..#.#....
		..#######.
		.#.#.###..
		.#..#.....
		..#....#.#
		#..#....#.
		.##.#..###
		##...#..#.
		.#....####
	");
	assert_eq!(best_location(&grid2), Location {coord: (5,8), n_reachable: 33});

	let grid3 = asteroids_from_str("
		#.#...#.#.
		.###....#.
		.#....#...
		##.#.#.#.#
		....#.#.#.
		.##..###.#
		..#...##..
		..##....##
		......#...
		.####.###.
	");
	assert_eq!(best_location(&grid3), Location {coord: (1,2), n_reachable: 35});

	let grid4 = asteroids_from_str("
		.#..#..###
		####.###.#
		....###.#.
		..###.##.#
		##.##.#.#.
		....###..#
		..#.#..#.#
		#..#.#.###
		.##...##.#
		.....#.#..
	");
	assert_eq!(best_location(&grid4), Location {coord: (6,3), n_reachable: 41});

	let grid5 = asteroids_from_str("
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
	assert_eq!(best_location(&grid5), Location {coord: (11,13), n_reachable: 210});
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
	let result = reachable_asteroids(&grid, (11,13));
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
	assert_approx_eq!(bearing((0, 0), (0, 1)), 0f32);
	assert_approx_eq!(bearing((0, 0), (1, 0)), PI / 2f32);
	assert_approx_eq!(bearing((0, 0), (0, -1)), PI);
	assert_approx_eq!(bearing((0, 0), (-1, 0)), PI / 2f32 * 3f32);
}

#[test]
fn test_bearing_45deg() {
	assert_approx_eq!(bearing((0, 0), (1, 1)), PI / 4f32);
	assert_approx_eq!(bearing((0, 0), (1, -1)), PI / 4f32 * 3f32);
	assert_approx_eq!(bearing((0, 0), (-1, -1)), PI / 4f32 * 5f32);
	assert_approx_eq!(bearing((0, 0), (-1, 1)), PI / 4f32 * 7f32);
}

#[test]
fn test_bearing_other() {
	assert_approx_eq!(bearing((0, 0), (2, 1)), 0.4636476);
	assert_approx_eq!(bearing((0, 0), (2, -1)), 2.0344440);
	assert_approx_eq!(bearing((0, 0), (-2, -1)), 3.6052404);
	assert_approx_eq!(bearing((0, 0), (-2, 1)), 5.1760368);
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

