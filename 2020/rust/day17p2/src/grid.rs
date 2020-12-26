#[cfg(test)]
use std::fmt;

/*
Provides parsing, nice debug formatting, and equality assertions with nice
debug formatting for grids of enums with no associated data.

To use:
1. Define the enum.
2. Implement the grid::FromChar and grid::ToStr traits for it.
3. Derive or implement the PartialEq trait for it.
4. Call grid::parse() to create grids.
5. Use assert_eq_grids!(actual, expected) in tests.
*/

#[macro_export]
macro_rules! assert_eq_grids {
	($left:expr, $right:expr) => (assert_eq!(
		crate::grid::wg(&$left),
		crate::grid::wg(&$right)
	))
}

pub trait FromChar {
	fn from_c(c: char) -> Self;
}

pub trait ToStr {
	fn to_s(&self) -> &'static str;
}

pub fn parse<T: FromChar>(input: &str) -> Vec<Vec<T>> {
	input
		.lines()
		.filter(|line| line.len() > 0)
		.map(|line| {
			line.chars()
				.map(T::from_c)
				.collect()
		})
		.collect()
}

#[cfg(test)]
#[derive(PartialEq)]
enum ExampleForTestParse {
	One,
	Two
}

#[cfg(test)]
impl FromChar for ExampleForTestParse {
	fn from_c(c: char) -> ExampleForTestParse {
		match c {
			'1' => ExampleForTestParse::One,
			'2' => ExampleForTestParse::Two,
			_ => panic!("nope")
		}
	}
}

#[cfg(test)]
impl ToStr for ExampleForTestParse {
	fn to_s(&self) -> &'static str {
		match self {
			ExampleForTestParse::One => "1",
			ExampleForTestParse::Two => "2",
		}
	}
}

#[test]
fn test_parse() {
	let input = "
12
21
";
	let expected = vec![
		vec![ExampleForTestParse::One, ExampleForTestParse::Two],
		vec![ExampleForTestParse::Two, ExampleForTestParse::One]
	];
	assert_eq_grids!(parse(input), expected);
}

#[cfg(test)]
#[derive(PartialEq)]
pub struct DebugGridWrapper<'a, T> {
	grid: &'a Vec<Vec<T>>
}

#[cfg(test)]
pub fn wg<T: ToStr>(grid: &Vec<Vec<T>>) -> DebugGridWrapper<T> {
	DebugGridWrapper { grid }
}

#[cfg(test)]
impl <T: ToStr> fmt::Debug for DebugGridWrapper<'_, T> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		f.write_str("\n")?;

		for row in self.grid {
			for cell in row {
				f.write_str(cell.to_s())?
			}

			f.write_str("\n")?
		}

		Ok(())
	}
}

