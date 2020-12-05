mod input;

fn main() {
	let highest = input::puzzle_input()
		.lines()
		.map(seat_id)
		.max().unwrap();
	println!("{}", highest);
	// 930
}

static MAX_ROW: u32 = 127;
static MAX_COL: u32 = 7;

fn seat_id(pass: &str) -> u32 {
	row(pass) * 8 + col(pass)
}

#[test]
fn test_seat_id() {
	assert_eq!(seat_id("FBFBBFFRLR"), 357);
}

fn row(pass: &str) -> u32 {
	bsp(0, MAX_ROW, pass.chars().take(7))
}

fn bsp<T: Iterator<Item = char>>(mut min: u32, mut max: u32, chars: T) -> u32 {
	for c in chars {
		assert!(min < max);
		let sz = (max - min) + 1;

		if c == 'F' || c == 'L' {
			max -= sz / 2;
		} else {
			min += sz / 2;
		}
	}

	assert!(min == max);
	min
}

#[test]
fn test_row() {
	assert_eq!(row("FBFBBFFRLR"), 44);
}

fn col(pass: &str) -> u32 {
	bsp(0, MAX_COL, pass.chars().skip(7))
}

#[test]
fn test_col() {
	assert_eq!(col("FBFBBFFRLR"), 5);
}
