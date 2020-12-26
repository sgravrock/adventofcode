use std::collections::HashMap;
use std::ops::{Index, IndexMut, RangeInclusive, Add};
use std::fmt;
/*
Provides infinite hypercube operations (get w/default, set, ranges) and
debug formatting for hypercubes of enums with no associated data.

To use, just set up the enum as described in the grid module.
*/

type Grid<T> = Vec<Vec<T>>;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Coord {
	pub x: isize,
	pub y: isize,
	pub z: isize,
	pub w: isize,
}

impl Add<Coord> for Coord {
	type Output = Self;

	fn add(self, other: Self) -> Self {
		Self {
			x: self.x + other.x,
			y: self.y + other.y,
			z: self.z + other.z,
			w: self.w + other.w,
		}
	}
}

#[derive(PartialEq, Eq)]
pub struct Hypercube<T> {
	pub cells: HashMap<Coord, T>,
	default: T
}


impl <T> Hypercube<T>
		where T: PartialEq,
		T: Copy,
		T: crate::grid::ToStr,
		T: crate::grid::FromChar {

	pub fn new(default: T) -> Hypercube<T> {
		Hypercube { cells: HashMap::new(), default }
	}

	pub fn parse_plane(input: &str, default: T) -> Hypercube<T> {
		 Hypercube::from_plane(crate::grid::parse(input), default)
	}

	pub fn from_plane(plane: Grid<T>, default: T) -> Hypercube<T> {
		let mut cells: HashMap<Coord, T> = HashMap::new();

		for (y, row) in plane.iter().enumerate() {
			for (x, cell) in row.iter().enumerate() {
				let c = Coord{x: x as isize, y: y as isize, z: 0, w: 0};
				cells.insert(c, *cell);
			}
		}

		Hypercube { cells, default }
	}
}

impl <T> Hypercube<T> {
	pub fn xrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|c| c.x).min().unwrap();
		let max = self.cells.keys().map(|c| c.x).max().unwrap();
		min..=max
	}

	pub fn yrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|c| c.y).min().unwrap();
		let max = self.cells.keys().map(|c| c.y).max().unwrap();
		min..=max
	}

	pub fn zrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|c| c.z).min().unwrap();
		let max = self.cells.keys().map(|c| c.z).max().unwrap();
		min..=max
	}

	pub fn wrange(&self) -> RangeInclusive<isize> {
		if self.cells.len() == 0 {
			return 0..=0;
		}

		let min = self.cells.keys().map(|c| c.w).min().unwrap();
		let max = self.cells.keys().map(|c| c.w).max().unwrap();
		min..=max
	}
}

impl <T> Index<Coord> for Hypercube<T> {
	type Output = T;

	fn index(&self, i: Coord) -> &Self::Output {
		self.cells.get(&i).unwrap_or(&self.default)
	}
}

impl <T> IndexMut<Coord> for Hypercube<T> where T: Copy {
	fn index_mut(&mut self, i: Coord) -> &mut Self::Output {
		self.cells.entry(i).or_insert(self.default)
	}
}

impl <T> fmt::Debug for Hypercube<T> where T: crate::grid::ToStr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_fmt(format_args!("\nx: {:?} y: {:?} z: {:?}\n\n",
			self.xrange(), self.yrange(), self.zrange()))?;

		for w in self.wrange() {
			for z in self.zrange() {
				f.write_fmt(format_args!("z={}, w={}\n", z, w))?;
				for y in self.yrange() {
					for x in self.xrange() {
						f.write_str(self[Coord{x, y, z, w}].to_s())?;
					}
	
					f.write_str("\n")?;
				}
	
				f.write_str("\n")?;
			}
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
fn test_hypercube_parse_plane() {
	let mut expected = Hypercube::new(ExampleForTests::One);
	expected[Coord{x: 0, y: 0, z: 0, w: 0}] = ExampleForTests::One;
	expected[Coord{x: 1, y: 0, z: 0, w: 0}] = ExampleForTests::Two;
	expected[Coord{x: 2, y: 0, z: 0, w: 0}] = ExampleForTests::One;
	expected[Coord{x: 0, y: 1, z: 0, w: 0}] = ExampleForTests::One;
	expected[Coord{x: 1, y: 1, z: 0, w: 0}] = ExampleForTests::One;
	expected[Coord{x: 2, y: 1, z: 0, w: 0}] = ExampleForTests::Two;
	expected[Coord{x: 0, y: 2, z: 0, w: 0}] = ExampleForTests::Two;
	expected[Coord{x: 1, y: 2, z: 0, w: 0}] = ExampleForTests::Two;
	expected[Coord{x: 2, y: 2, z: 0, w: 0}] = ExampleForTests::Two;
	let actual = Hypercube::parse_plane("
121
112
222
", ExampleForTests::One);
	assert_eq!(actual, expected);
}



#[test]
fn test_hypercube_index() {
	let initial_plane = crate::grid::parse("12\n21");
	let mut hypercube = Hypercube::from_plane(initial_plane, ExampleForTests::One);
	hypercube[Coord{x: -1, y: -2, z: -3, w: 0}] = ExampleForTests::Two;

	assert_eq!(
		hypercube[Coord{x: -1, y: -2, z: -3, w: 0}],
		ExampleForTests::Two
	);
	// default
	assert_eq!(
		hypercube[Coord{x: -1, y: -1, z: -1, w: 0}],
		ExampleForTests::One
	);
	assert_eq!(hypercube[Coord{x: 0, y: 0, z: 0, w: 0}], ExampleForTests::One);
	assert_eq!(hypercube[Coord{x: 1, y: 0, z: 0, w: 0}], ExampleForTests::Two);
	assert_eq!(hypercube[Coord{x: 0, y: 1, z: 0, w: 0}], ExampleForTests::Two);
	assert_eq!(hypercube[Coord{x: 1, y: 1, z: 0, w: 0}], ExampleForTests::One);
}
