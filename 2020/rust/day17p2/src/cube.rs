use std::collections::HashMap;
use std::ops::{Index, IndexMut, RangeInclusive, Add};
use std::fmt;
/*
Provides infinite cube operations (get w/default, set, ranges) and
debug formatting for cubes of enums with no associated data.

To use, just set up the enum as described in the grid module.
*/

type Grid<T> = Vec<Vec<T>>;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Coord {
	pub x: isize,
	pub y: isize,
	pub z: isize,
}

impl Add<Coord> for Coord {
	type Output = Self;

	fn add(self, other: Self) -> Self {
		Self {
			x: self.x + other.x,
			y: self.y + other.y,
			z: self.z + other.z,
		}
	}
}

#[derive(PartialEq, Eq)]
pub struct Cube<T> {
	pub cells: HashMap<Coord, T>,
	default: T
}


impl <T> Cube<T>
		where T: PartialEq,
		T: Copy,
		T: crate::grid::ToStr,
		T: crate::grid::FromChar {

	pub fn new(default: T) -> Cube<T> {
		Cube { cells: HashMap::new(), default }
	}

	pub fn parse_plane(input: &str, default: T) -> Cube<T> {
		 Cube::from_plane(crate::grid::parse(input), default)
	}

	pub fn from_plane(plane: Grid<T>, default: T) -> Cube<T> {
		let mut cells: HashMap<Coord, T> = HashMap::new();

		for (y, row) in plane.iter().enumerate() {
			for (x, cell) in row.iter().enumerate() {
				cells.insert(Coord{x: x as isize, y: y as isize, z: 0}, *cell);
			}
		}

		Cube { cells, default }
	}
}

impl <T> Cube<T> {
	pub fn xrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|k| k.x).min().unwrap();
		let max = self.cells.keys().map(|k| k.x).max().unwrap();
		min..=max
	}

	pub fn yrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|k| k.y).min().unwrap();
		let max = self.cells.keys().map(|k| k.y).max().unwrap();
		min..=max
	}

	pub fn zrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|k| k.z).min().unwrap();
		let max = self.cells.keys().map(|k| k.z).max().unwrap();
		min..=max
	}
}

impl <T> Index<Coord> for Cube<T> {
	type Output = T;

	fn index(&self, i: Coord) -> &Self::Output {
		self.cells.get(&i).unwrap_or(&self.default)
	}
}

impl <T> IndexMut<Coord> for Cube<T> where T: Copy {
	fn index_mut(&mut self, i: Coord) -> &mut Self::Output {
		self.cells.entry(i).or_insert(self.default)
	}
}

impl <T> fmt::Debug for Cube<T> where T: crate::grid::ToStr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!("\nx: {:?} y: {:?} z: {:?}\n\n",
			self.xrange(), self.yrange(), self.zrange()))?;

		for z in self.zrange() {
			f.write_fmt(format_args!("z={}\n", z))?;
			for y in self.yrange() {
				for x in self.xrange() {
					f.write_str(self[Coord{x, y, z}].to_s())?;
				}

				f.write_str("\n")?;
			}

			f.write_str("\n")?;
		}

		Ok(())
	}
}


#[cfg(test)]
#[derive(PartialEq, Debug, Clone, Copy)]
enum ExampleForTests {
	One,
	Two
}

#[cfg(test)]
impl crate::grid::FromChar for ExampleForTests {
	fn from_c(c: char) -> ExampleForTests {
		match c {
			'1' => ExampleForTests::One,
			'2' => ExampleForTests::Two,
			_ => panic!("{} isn't valid", c)
		}
	}
}

#[cfg(test)]
impl crate::grid::ToStr for ExampleForTests {
	fn to_s(&self) -> &'static str {
		match self {
			ExampleForTests::One => "1",
			ExampleForTests::Two => "2",
		}
	}
}

#[test]
fn test_cube_parse_plane() {
	let mut expected = Cube::new(ExampleForTests::One);
	expected[Coord{x: 0, y: 0, z: 0}] = ExampleForTests::One;
	expected[Coord{x: 1, y: 0, z: 0}] = ExampleForTests::Two;
	expected[Coord{x: 2, y: 0, z: 0}] = ExampleForTests::One;
	expected[Coord{x: 0, y: 1, z: 0}] = ExampleForTests::One;
	expected[Coord{x: 1, y: 1, z: 0}] = ExampleForTests::One;
	expected[Coord{x: 2, y: 1, z: 0}] = ExampleForTests::Two;
	expected[Coord{x: 0, y: 2, z: 0}] = ExampleForTests::Two;
	expected[Coord{x: 1, y: 2, z: 0}] = ExampleForTests::Two;
	expected[Coord{x: 2, y: 2, z: 0}] = ExampleForTests::Two;
	let actual = Cube::parse_plane("
121
112
222
", ExampleForTests::One);
	assert_eq!(actual, expected);
}



#[test]
fn test_cube_index() {
	let initial_plane = crate::grid::parse("12\n21");
	let mut cube = Cube::from_plane(initial_plane, ExampleForTests::One);
	cube[Coord{x: -1, y: -2, z: -3}] = ExampleForTests::Two;

	assert_eq!(cube[Coord{x: -1, y: -2, z: -3}], ExampleForTests::Two);
	assert_eq!(cube[Coord{x: -1, y: -1, z: -1}], ExampleForTests::One); // default
	assert_eq!(cube[Coord{x: 0, y: 0, z: 0}], ExampleForTests::One);
	assert_eq!(cube[Coord{x: 1, y: 0, z: 0}], ExampleForTests::Two);
	assert_eq!(cube[Coord{x: 0, y: 1, z: 0}], ExampleForTests::Two);
	assert_eq!(cube[Coord{x: 1, y: 1, z: 0}], ExampleForTests::One);
}
