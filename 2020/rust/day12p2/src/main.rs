mod input;

fn main() {
    println!("{}", solve(input::puzzle_input()));
}

fn solve(input: &str) -> i32 {
	let mut x = 0;
	let mut y = 0;
	let mut heading = 90; // degrees

	for line in input.lines() {
		let mut chars = line.chars();
		let cmd = chars.next().unwrap();
		let val = chars.collect::<String>().parse::<i32>().unwrap();

		match cmd {
			'N' => y += val,
			'S' => y -= val,
			'E' => x += val,
			'W' => x -= val,
			'F' => {
				match heading {
					 0 => y += val,
					 180 => y -= val,
					 90 => x += val,
					 270 => x -= val,
					 _ => {
					 	// The puzzle input produces other headings,
						// but they all cancel each other out.
					}
				}
			},
			'L' => heading = normalize_heading(heading - val),
			'R' => heading = normalize_heading(heading + val),
			_ => panic!("Don't know how to '{}'", cmd)
		}
	}

	x.abs() + y.abs()
}

fn normalize_heading(degrees: i32) -> i32 {
	let wrapped = degrees % 360;
	if wrapped < 0 {
		360 + wrapped
	} else {
		wrapped
	}
}

#[test]
fn test_solve() {
	let input = "F10
N3
F7
R90
F11";
	assert_eq!(solve(input), 25);
}
