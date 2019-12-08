#[macro_use] extern crate text_io;
mod input;
mod machine;
mod debugger;
use machine::{Machine, MachineState};
use debugger::debug;
extern crate permutohedron;

fn main() {
	let program = input::puzzle_input();
	println!("{}", find_max_thruster_signal(program).max_thruster_signal);
}

#[derive(Debug, PartialEq)]
struct SearchResult {
	max_thruster_signal: i32,
	phase_settings: Vec<i32>
}

fn find_max_thruster_signal(program: Vec<i32>) -> SearchResult {
	all_possible_phase_settings()
		.iter()
		.map(|phase_settings| {
			SearchResult {
				max_thruster_signal: thruster_signal(&program, &phase_settings),
				phase_settings: phase_settings.clone()
			}
		})
		.max_by_key(|sr| sr.max_thruster_signal)
		.unwrap()
}

#[test]
fn test_find_max_thruster_signal() {
	assert_eq!(
		find_max_thruster_signal(
			vec![3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,
				28,-1,28,1005,28,6,99,0,0,5]
		),
		SearchResult {
			max_thruster_signal: 139629729,
			phase_settings: vec![9,8,7,6,5]
		}
	);
	assert_eq!(
		find_max_thruster_signal(
			vec![3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,
				1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,
				2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
		),
		SearchResult {
			max_thruster_signal: 18216,
			phase_settings: vec![9,7,8,5,6]
		}
	);
}

fn all_possible_phase_settings() -> Vec<Vec<i32>> {
	let mut values = [5, 6, 7, 8, 9];
	let mut result: Vec<Vec<i32>> = vec![];
	permutohedron::heap_recursive(&mut values, |s| { result.push(s.to_vec()) });
	result
}

fn thruster_signal(program: &Vec<i32>, phase_settings: &Vec<i32>) -> i32 {
	let mut machines: Vec<Machine> = phase_settings
		.iter()
		.map(|phase_setting| {
			let mut m = Machine::new(program.clone());
			m.input.enqueue(*phase_setting);
			m
		})
		.collect();
	
	machines[0].input.enqueue(0);

	loop {
		for i in 0..machines.len() {
			machines[i].execute().unwrap();
			let output = machines[i].output.dequeue().unwrap();

			if i == machines.len() - 1 {
				if machines[i].state == MachineState::Halted {
					return output;
				} else {
					machines[0].input.enqueue(output);
				}
			} else {
				machines[i + 1].input.enqueue(output);
			}
		}
	}
}
