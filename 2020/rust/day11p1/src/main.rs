mod input;
use std::fmt;

fn main() {
	println!("{}", solve(parse_grid(input::puzzle_input())));
	// 2346
}

#[derive(PartialEq, Clone, Copy)]
enum Cell {
	Floor,
	Empty,
	Occupied
}

impl Cell {
	fn from_c(c: char) -> Cell {
		match c {
			'.' => Cell::Floor,
			'L' => Cell::Empty,
			'#' => Cell::Occupied,
			_ => panic!("Input char '{}' is not a valid Cell", c)
		}
	}
}

impl fmt::Debug for Cell {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str(match self {
			Cell::Floor => ".",
			Cell::Empty => "L",
			Cell::Occupied => "#",
		})
	}
}

fn solve(grid: Vec<Vec<Cell>>) -> usize {
	advance_until_settled(grid)
		.iter()
		.flat_map(|row| row.iter())
		.filter(|c| **c == Cell::Occupied)
		.count()
}

#[test]
fn test_solve() {
	let initial = parse_grid("
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
");
	assert_eq!(solve(initial), 37);
}

fn advance_until_settled(mut grid: Vec<Vec<Cell>>) -> Vec<Vec<Cell>> {
	loop {
		let (next_grid, any_changed) = tick(grid);

		if any_changed {
			grid = next_grid;
		} else {
			return next_grid;
		}
	}
}


fn tick(grid: Vec<Vec<Cell>>) -> (Vec<Vec<Cell>>, bool) {
	let mut any_changed = false;
	let result_grid = (0..grid.len())
		.map(|i| {
			(0..grid[i].len())
				.map(|j| {
					let c = grid[i][j];
					if c == Cell::Empty && adj_occupied(&grid, i, j) == 0 {
						any_changed = true;
						Cell::Occupied
					} else if c == Cell::Occupied && adj_occupied(&grid, i, j) >= 4 {
						any_changed = true;
						Cell::Empty
					} else {
						c
					}
				})
				.collect()
		})
		.collect();
	(result_grid, any_changed)
}

fn adj_occupied(grid: &Vec<Vec<Cell>>, i: usize, j: usize) -> usize {
	static DELTAS: &[(i32, i32); 8] = &[
		(-1, -1), (0, -1), (1, -1),
		(-1, 0), (1, 0),
		(-1, 1), (0, 1), (1, 1)
	];

	DELTAS.iter()
		.filter(|d| {
			let ni = i as i32 + d.0;
			let nj = j as i32 + d.1;

			if ni < 0 || nj < 0 || ni >= grid.len() as i32 || nj >= grid[i].len() as i32 {
				false // Out of bounds
			} else {
				grid[ni as usize][nj as usize] == Cell::Occupied
			}
		})
		.count()
}

#[test]
fn test_tick() {
	let initial = parse_grid("
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
");
	let tick1 = tick(initial).0;
	assert_eq!(wg(&tick1), wg(&parse_grid("
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
")));
	let tick2 = tick(tick1).0;
	assert_eq!(wg(&tick2), wg(&parse_grid("
#.LL.L#.##
#LLLLLL.L#
L.L.L..L..
#LLL.LL.L#
#.LL.LL.LL
#.LLLL#.##
..L.L.....
#LLLLLLLL#
#.LLLLLL.L
#.#LLLL.##
")));	
}


fn parse_grid(input: &str) -> Vec<Vec<Cell>> {
	input
		.lines()
		.filter(|line| line.len() > 0)
		.map(|line| {
			line.chars()
				.map(Cell::from_c)
				.collect()
		})
		.collect()
}

#[test]
fn test_parse_grid() {
	let input = "
#.
L#
";
	let expected = vec![
		vec![Cell::Occupied, Cell::Floor],
		vec![Cell::Empty, Cell::Occupied],
	];
	assert_eq!(wg(&parse_grid(input)), wg(&expected));
}


#[cfg(test)]
#[derive(PartialEq)]
struct DebugGridWrapper<'a> {
	grid: &'a Vec<Vec<Cell>>
}

#[cfg(test)]
fn wg(grid: &Vec<Vec<Cell>>) -> DebugGridWrapper {
	DebugGridWrapper { grid }
}

#[cfg(test)]
impl fmt::Debug for DebugGridWrapper<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("\n")?;

		for row in self.grid {
			for cell in row {
				cell.fmt(f)?
			}

			f.write_str("\n")?
		}

		Ok(())
	}
}
