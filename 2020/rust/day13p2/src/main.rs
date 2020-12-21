mod input;

fn main() {
	println!("{}", solve(input::puzzle_input()));
}

fn solve(input: &str) -> u32 {
	let bus_ids: Vec<Option<u32>> = input
		.split(',')
		.map(|bus_id| bus_id.parse::<u32>().ok())
		.collect();

	let mut t = bus_ids[0].unwrap();

	while !valid_solution(&bus_ids, t) {
		t += bus_ids[0].unwrap();
	}

	t
}

fn valid_solution(bus_ids: &Vec<Option<u32>>, time: u32) -> bool {
	for i in 1..bus_ids.len() {
		match bus_ids[i] {
			None => {},
			Some(id) => {
				if (time + i as u32) % id != 0 {
					return false;
				}
			}
		}
	}

	true
}

#[test]
fn test_solve() {
	assert_eq!(solve("7,13,x,x,59,x,31,19"), 1068781);
}
