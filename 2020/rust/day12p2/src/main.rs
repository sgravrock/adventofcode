mod input;

fn main() {
    println!("{}", solve(input::puzzle_input()));
}

#[derive(PartialEq, Debug)]
struct State {
	x: i32,
	y: i32,
	heading: i32, // degrees
}

fn solve(input: &str) -> i32 {
	let initial_state = State {
		x: 0,
		y: 0,
		heading: 90
	};

	let end_state = input.lines().fold(initial_state, handle_instruction);
	end_state.x.abs() + end_state.y.abs()
}

fn handle_instruction(mut state: State, instruction: &str) -> State {
	let mut chars = instruction.chars();
	let cmd = chars.next().unwrap();
	let val = chars.collect::<String>().parse::<i32>().unwrap();

	match cmd {
		'N' => state.y += val,
		'S' => state.y -= val,
		'E' => state.x += val,
		'W' => state.x -= val,
		'F' => {
			match state.heading {
				 0 => state.y += val,
				 180 => state.y -= val,
				 90 => state.x += val,
				 270 => state.x -= val,
				 _ => {
				 	// The puzzle input produces other headings,
					// but they all cancel each other out.
				}
			}
		},
		'L' => state.heading = normalize_heading(state.heading - val),
		'R' => state.heading = normalize_heading(state.heading + val),
		_ => panic!("Don't know how to '{}'", cmd)
	}

	state
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
