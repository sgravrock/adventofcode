mod input;
mod grid;

fn main() {
	println!("{}", solve(grid::parse(input::puzzle_input())));
	// 2111
}


#[derive(PartialEq, Clone, Copy)]
enum Cell {
	Floor,
	Empty,
	Occupied
}

impl grid::FromChar for Cell {
	fn from_c(c: char) -> Cell {
		match c {
			'.' => Cell::Floor,
			'L' => Cell::Empty,
			'#' => Cell::Occupied,
			_ => panic!("Input char '{}' is not a valid Cell", c)
		}
	}
}

impl grid::ToStr for Cell {
	fn to_s(&self) -> &'static str {
		match self {
			Cell::Floor => ".",
			Cell::Empty => "L",
			Cell::Occupied => "#",
		}
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
	let initial = grid::parse("
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
	assert_eq!(solve(initial), 26);
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
					if c == Cell::Empty && visible_occupied(&grid, i, j) == 0 {
						any_changed = true;
						Cell::Occupied
					} else if c == Cell::Occupied
							&& visible_occupied(&grid, i, j) >= 5 {
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

fn visible_occupied(grid: &Vec<Vec<Cell>>, i: usize, j: usize) -> usize {
	static DIRECTIONS: &[(isize, isize); 8] = &[
		(-1, -1), (0, -1), (1, -1),
		(-1, 0), (1, 0),
		(-1, 1), (0, 1), (1, 1)
	];

	DIRECTIONS.iter()
		.filter(|d| can_see_occupied(&grid, i, j, **d))
		.count()
}

fn can_see_occupied(
	grid: &Vec<Vec<Cell>>,
	i: usize,
	j: usize,
	dir: (isize, isize)
)-> bool {
	let mut ci = i as isize + dir.0;
	let mut cj = j as isize + dir.1;

	while in_bounds(&grid, ci, cj) {
		match grid[ci as usize][cj as usize] {
			Cell::Occupied => return true,
			Cell::Empty => return false,
			Cell::Floor => {}
		}

		ci += dir.0;
		cj += dir.1;
	}

	false
}

fn in_bounds(grid: &Vec<Vec<Cell>>, i: isize, j: isize) -> bool {
	i >= 0
		&& j >= 0
		&& i < grid.len() as isize
		&& j < grid[i as usize].len() as isize 
}

#[test]
fn test_tick() {
	let initial = grid::parse("
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
	assert_eq_grids!(tick1, grid::parse("
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
"));
	let tick2 = tick(tick1).0;
	assert_eq_grids!(tick2, grid::parse("
#.LL.LL.L#
#LLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLL#
#.LLLLLL.L
#.LLLLL.L#
"));	
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
	assert_eq_grids!(grid::parse(input), expected);
}
