use itertools::Itertools;
mod input;

fn main() {
	println!("{}", solve(input::puzzle_input()));
	// 4782
}

fn solve(input: &str) -> u32 {
	let mut lines = input.lines();
	let earliest_departure_time: u32 = lines.next().unwrap().parse().unwrap();
	lines.next().unwrap()
		.split(',')
		.filter(|bus_id| *bus_id != "x")
		.map(|bus_id| bus_id.parse::<u32>().unwrap())
		.map(|bus_id| (bus_id, next_departure(bus_id, earliest_departure_time)))
		.sorted_by(|a, b| Ord::cmp(&a.1, &b.1)) // by departure time
		.map(|(bus_id, departure_time)| {
			bus_id * (departure_time - earliest_departure_time)
		})
		.next()
		.unwrap()
}

fn next_departure(bus_id: u32, earliest_departure_time: u32) -> u32 {
	earliest_departure_time + bus_id - earliest_departure_time % bus_id
}

#[test]
fn test_solve() {
	let input = "939
7,13,x,x,59,x,31,19";
	assert_eq!(solve(input), 295);
}
