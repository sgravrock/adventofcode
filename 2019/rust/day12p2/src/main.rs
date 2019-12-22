mod input;
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use regex::Regex;
use num::integer::lcm;

fn main() {
	let axes = parse_moons(input::puzzle_input());
	let steps = find_repeat(axes);
	println!("The universe repeats after {} steps.", steps);
}

fn find_repeat(axes: Vec<Vec<MoonAxis>>) -> u64 {
	let x = find_repeat_for_axis(axes[0].clone());
	let y = find_repeat_for_axis(axes[1].clone());
	let z = find_repeat_for_axis(axes[2].clone());
	lcm(x, lcm(y, z))
}

fn find_repeat_for_axis(mut axis: Vec<MoonAxis>) -> u64 {
	let mut seen = HashSet::new();
	seen.insert(hash(&axis));
	let mut i = 0u64;

	loop {
		step(&mut axis);
		i += 1;

		if !seen.insert(hash(&axis)) {
			return i;
		}
	}
}

 
fn hash(axis: &Vec<MoonAxis>) -> u64 {
	let mut hasher = DefaultHasher::new();

	for ma in axis {
		hasher.write_i32(ma.pos);
		hasher.write_i32(ma.vel);
	}

	hasher.finish()
}

#[derive(PartialEq, Hash, Copy, Clone, Debug)]
struct MoonAxis {
	pos: i32,
	vel: i32,
}

fn parse_moons(input: &str) -> Vec<Vec<MoonAxis>> {
	let re = Regex::new("<x=([0-9\\-]+), y=([0-9\\-]+), z=([0-9\\-]+)>")
		.unwrap();
	let mut x_axis: Vec<MoonAxis> = Vec::new();
	let mut y_axis: Vec<MoonAxis> = Vec::new();
	let mut z_axis: Vec<MoonAxis> = Vec::new();

	for line in input.lines() {
		let caps = re.captures(line).unwrap();
		let x = caps.get(1).unwrap().as_str().parse::<i32>().unwrap();
		let y = caps.get(2).unwrap().as_str().parse::<i32>().unwrap();
		let z = caps.get(3).unwrap().as_str().parse::<i32>().unwrap();
		x_axis.push(MoonAxis { pos: x, vel: 0 });
		y_axis.push(MoonAxis { pos: y, vel: 0 });
		z_axis.push(MoonAxis { pos: z, vel: 0 });
	}
	
	vec![x_axis, y_axis, z_axis]
}


fn step(mut axis: &mut Vec<MoonAxis>) {
	apply_gravity(&mut axis);
	apply_velocity(&mut axis);
}

fn apply_gravity(axis: &mut Vec<MoonAxis>) {
	for (i, j) in each_pair(axis.len()) {
		axis[i].vel +=
			if axis[i].pos < axis[j].pos {
				1
			} else if axis[i].pos > axis[j].pos {
				-1
			} else {
				0
			};
	}
}	

fn apply_velocity(axis: &mut Vec<MoonAxis>) {
	for ma in axis {
		ma.pos += ma.vel;
	}
}

fn each_pair(n: usize) -> Vec<(usize, usize)> {
	let mut result = Vec::new();

	for i in 0..n {
		for j in (i + 1)..n {
			result.push((i, j));
			result.push((j, i));
		}
	}

	result
}

#[test]
fn test_step() {
	let mut axes = parse_moons("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>");

	let expected_x = vec![
		MoonAxis {pos: 2, vel: 3},
		MoonAxis {pos: 3, vel: 1},
		MoonAxis {pos: 1, vel: -3},
		MoonAxis {pos: 2, vel: -1},
	];
	step(&mut axes[0]);
	assert_eq!(axes[0], expected_x);

	let expected_y = vec![
		MoonAxis {pos: -1, vel: -1},
		MoonAxis {pos: -7, vel: 3},
		MoonAxis {pos: -7, vel: 1},
		MoonAxis {pos: 2, vel: -3},
	];
	step(&mut axes[1]);
	assert_eq!(axes[1], expected_y);

	let expected_z = vec![
		MoonAxis {pos: 1, vel: -1},
		MoonAxis {pos: -4, vel: 3},
		MoonAxis {pos: 5, vel: -3},
		MoonAxis {pos: 0, vel: 1},
	];
	step(&mut axes[2]);
	assert_eq!(axes[2], expected_z);
}

#[test]
fn test_find_repeat() {
	let moons = parse_moons("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>");

	assert_eq!(find_repeat(moons), 2772);
}

#[test]
fn test_find_repeat_long() {
	let moons = parse_moons("<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>");

	assert_eq!(find_repeat(moons), 4686774924);
}

