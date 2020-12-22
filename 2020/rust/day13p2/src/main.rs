mod input;

fn main() {
	println!("{}", solve(input::puzzle_input()));
	// 1118684865113056
}

fn solve(input: &str) -> u64 {
	let bus_ids: Vec<Option<u64>> = input
		.split(',')
		.map(|bus_id| bus_id.parse::<u64>().ok())
		.collect();

	let largest_i = index_of_largest(&bus_ids);
	let largest_id = bus_ids[largest_i].unwrap();
	let mut t = largest_id;

	while !valid_solution(&bus_ids, t - largest_i as u64) {
		t += largest_id;
	}

	t - largest_i as u64
}

fn index_of_largest(bus_ids: &Vec<Option<u64>>) -> usize {
	let mut result = 0;

	for i in 1..bus_ids.len() {
		match bus_ids[i] {
			None => {},
			Some(id) => {
				if id > bus_ids[result].unwrap() {
					result = i;
				}
			}
		}
	}

	result
}

fn valid_solution(bus_ids: &Vec<Option<u64>>, time: u64) -> bool {
	for i in 0..bus_ids.len() {
		match bus_ids[i] {
			None => {},
			Some(id) => {
				if (time + i as u64) % id != 0 {
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
