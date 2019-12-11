mod input;
use std::mem::swap;
#[cfg(test)] use std::collections::HashSet;
#[cfg(test)] use std::iter::FromIterator;
use regex::Regex;

fn main() {
	let grid = str_to_grid(input::puzzle_input());
	println!("{:?}", best_location(&grid));
}

#[derive(PartialEq, Debug)]
struct Grid {
	cells: Vec<bool>,
	width: usize,
	height: usize,
}

type Coord = (usize, usize);

#[derive(PartialEq, Debug)]
struct Location {
	coord: Coord,
	n_reachable: usize
}

fn best_location(grid: &Grid) -> Location {
	all_coords(&grid)
		.into_iter()
		.filter(|obs| grid.cells[obs.1 * grid.width + obs.0])
		.map(|obs| Location {
			coord: obs,
			n_reachable: reachable_asteroids(&grid, obs).len()
		})
		.max_by_key(|loc| loc.n_reachable)
		.unwrap()
}

fn reachable_asteroids(grid: &Grid, observer: Coord) -> Vec<Coord> {
	all_coords(&grid)
		.into_iter()
		.filter(|dest| {
			*dest != observer &&
				grid.cells[dest.1 * grid.width + dest.0] &&
				is_reachable(&grid, observer, *dest)
		})
		.collect()
}

fn all_coords(grid: &Grid) -> Vec<Coord> {
	(0..grid.height)
		.flat_map(|y| {
			(0..grid.width).map(move |x| (x, y))
		})
		.collect()
}

fn is_reachable(grid: &Grid, mut observer: Coord, mut dest: Coord) -> bool {
	assert!(observer.0 < grid.width);
	assert!(observer.1 < grid.cells.len() / grid.width);
	assert!(dest.0 < grid.width);
	assert!(dest.1 < grid.cells.len() / grid.width);

	// TODO: Unify this with the general case below somehow
	if observer.0 == dest.0 {
		if observer.1 > dest.1 {
			swap(&mut observer, &mut dest);
		}

		for y2 in (observer.1 + 1)..dest.1 {
			if grid.cells[y2 * grid.width + observer.0] {
				return false;
			}
		}
	} else {
		if observer.0 > dest.0 {
			swap(&mut observer, &mut dest);
		}
	
		let rise = dest.1 as f32 - observer.1 as f32;
		let slope = rise / (dest.0 - observer.0) as f32;
	
		for x in (observer.0 + 1)..dest.0 {
			let dx = x - observer.0;
			let dy = (dx as f32) * slope;
	
			if (dy - dy.round()).abs() < 0.01 {
				let y = (observer.1 as isize + dy.round() as isize) as usize;
				if grid.cells[y * grid.width + x] {
					return false;
				}
			}
		}
	}

	true
}

fn str_to_grid(input: &str) -> Grid {
	let whitespace = Regex::new(r"[\s]+").unwrap();
	let width = match whitespace.replace(input, "").find('\n') {
		Some(n) => n,
		None => input.len()
	};
	let cells: Vec<bool> = whitespace.replace_all(input, "")
		.chars()
		.map(|c| match c {
			'.' => false,
			'#' => true,
			_ => panic!("Unexpected input character: {}", c)
		})
		.collect();
	let height = cells.len() / width;
	Grid { cells, width, height }
}


#[test]
fn test_best_location() {
	let grid1 = str_to_grid("
		.#..#
		.....
		#####
		....#
		...##
	");
	assert_eq!(best_location(&grid1), Location {coord: (3, 4), n_reachable: 8});

	let grid2 = str_to_grid("
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

	let grid3 = str_to_grid("
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

	let grid4 = str_to_grid("
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

	let grid5 = str_to_grid("
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
	let grid = str_to_grid("
		.#..#
		.....
		#####
		....#
		...##
	");
	let expected = vec![
		(4, 0),
		(0, 2), (1, 2), (2, 2), (3, 2), (4, 2),
		(4, 3),
		(4, 4)
	];
	assert_eq!(
		vec_to_set(reachable_asteroids(&grid, (3, 4))),
		vec_to_set(expected)
	);
}

#[test]
fn test_is_reachable_obviously_unblocked() {
	let grid = str_to_grid("
		#..
		...
	");
	assert_eq!(is_reachable(&grid, (2, 1), (0, 0)), true);
}

#[test]
fn test_is_reachable_obviously_blocked_v() {
	let grid = str_to_grid("
		#
		#
		.
	");
	assert_eq!(is_reachable(&grid, (0, 2), (0, 0)), false);
}

#[test]
fn test_is_reachable_obviously_blocked_h() {
	let grid = str_to_grid("##.");
	assert_eq!(is_reachable(&grid, (2, 0), (0, 0)), false);
}

#[test]
fn test_is_reachable_obviously_blocked_d() {
	let grid = str_to_grid("
		#..
		.#.
		...
	");
	assert_eq!(is_reachable(&grid, (0, 0), (2, 2)), false);
}

// TODO mark all the srcs as #
#[test]
fn test_is_reachable_blocked_at_an_angle() {
	let grid = str_to_grid("
		#......
		...#...
		.......
	");
	assert_eq!(is_reachable(&grid, (6, 2), (0, 0)), false);
}

#[test]
fn test_is_reachable_not_blocked_at_an_angle() {
	let grid = str_to_grid("
		##.#...
		#.#.#..
		...#...
	");
	assert_eq!(is_reachable(&grid, (6, 2), (0, 0)), true);
}

#[test]
fn test_str_to_grid() {
	let input = "
		.#..#
		#####
		...##
		";
	let expected = Grid {
		cells: vec![
			false, true, false, false, true,
			true, true, true, true, true,
			false, false, false, true, true,
		],
		width: 5,
		height: 3
	};
	assert_eq!(str_to_grid(input), expected);
}

#[cfg(test)]
fn vec_to_set(vec: Vec<Coord>) -> HashSet<Coord> {
	HashSet::from_iter(vec.iter().cloned())
}
