mod input;
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::fmt;
use regex::Regex;
use num::integer::lcm;

fn main() {
	let mut moons = Moons::parse(input::puzzle_input());
	let steps = find_repeat(&mut moons);
	println!("The universe repeats after {} steps.", steps);
}

fn find_repeat(mut moons: &mut Moons) -> u64 {
	let periods: Vec<u64> = [0, 1, 2].iter()
		.map(|axis| find_repeat_for_axis(&mut moons, *axis))
		.collect();

	println!("Cycles: {:?}", periods);
	lcm(periods[0], lcm(periods[1], periods[2]))
}

fn find_repeat_for_axis(moons: &mut Moons, axis: usize) -> u64 {
	let mut seen = HashSet::new();
	seen.insert(hash(moons, axis));
	let mut i = 0u64;

	loop {
		moons.step(axis);
		i += 1;

		if !seen.insert(hash(moons, axis)) {
			return i;
		}
	}
}

 
fn hash(moons: &Moons, axis: usize) -> u64 {
	let mut hasher = DefaultHasher::new();

	for m in &moons.v {
		hasher.write_i32(m.pos[axis]);
		hasher.write_i32(m.vel[axis]);
	}

	hasher.finish()
}

#[derive(PartialEq, Hash, Clone)]
struct Moon {
	pos: Vec<i32>,
	vel: Vec<i32>,
}

impl fmt::Debug for Moon {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "pos=<x={}, y={}, z={}>, vel=<x={}, y={}, z={}>",
			self.pos[0], self.pos[1], self.pos[2],
			self.vel[0], self.vel[1], self.vel[2]
		)
	}
}

#[derive(PartialEq, Hash)]
struct Moons {
	v: Vec<Moon>
}

impl Moons {
	fn parse(input: &str) -> Moons {
		let re = Regex::new("<x=([0-9\\-]+), y=([0-9\\-]+), z=([0-9\\-]+)>")
			.unwrap();
	
		let v = input.lines()
			.map(|line| {
				let caps = re.captures(line).unwrap();
				let x = caps.get(1).unwrap().as_str().parse::<i32>().unwrap();
				let y = caps.get(2).unwrap().as_str().parse::<i32>().unwrap();
				let z = caps.get(3).unwrap().as_str().parse::<i32>().unwrap();
				Moon {
					pos: vec![x, y, z],
					vel: vec![0, 0, 0]
				}
			})
			.collect();
		
		Moons { v }
	}

	fn step(&mut self, axis: usize) {
		self.apply_gravity(axis);
		self.apply_velocity(axis);
	}

	fn apply_gravity(&mut self, axis: usize) {
		for (i, j) in each_pair(self.v.len()) {
			self.v[i].vel[axis] +=
				if self.v[i].pos[axis] < self.v[j].pos[axis] {
					1
				} else if self.v[i].pos[axis] > self.v[j].pos[axis] {
					-1
				} else {
					0
				};
		}
	}
	
	fn apply_velocity(&mut self, axis: usize) {
		for moon in &mut self.v {
			moon.pos[axis] += moon.vel[axis];
		}
	}

}

impl fmt::Debug for Moons {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "[\n")?;

		for m in &self.v {
			write!(f, "   ")?;
			m.fmt(f)?;
			write!(f, "\n")?;
		}

		write!(f, "]\n")
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

#[cfg(test)]
fn parse_state(input: Vec<&str>) -> Moons {
	let num_pat = "[\\s]*(-?[\\d]+)";
	let triple_pat = format!("<x={}, y={}, z={}>", num_pat, num_pat, num_pat);
	let pat = format!("pos={}, vel={}", triple_pat, triple_pat);
	let re = Regex::new(&pat).unwrap();

	let v = input.iter()
		.map(|line| {
			let caps = re.captures(line).unwrap();
			let px = caps.get(1).unwrap().as_str().parse::<i32>().unwrap();
			let py = caps.get(2).unwrap().as_str().parse::<i32>().unwrap();
			let pz = caps.get(3).unwrap().as_str().parse::<i32>().unwrap();
			let vx = caps.get(4).unwrap().as_str().parse::<i32>().unwrap();
			let vy = caps.get(5).unwrap().as_str().parse::<i32>().unwrap();
			let vz = caps.get(6).unwrap().as_str().parse::<i32>().unwrap();
		
			Moon {
				pos: vec![px, py, pz],
				vel: vec![vx, vy, vz]
			}
		})
		.collect();

	Moons { v }
}

#[test]
fn test_parse_state() {
	let input = vec!["pos=<x= 30, y= -8, z=  3>, vel=<x=  3, y=  -3, z=  0>"];
	let expected = Moons { v: vec![
		Moon {
			pos: vec![30, -8, 3 ],
			vel: vec![3, -3, 0]
		}
	]};
	assert_eq!(parse_state(input), expected);
}


#[test]
fn test_moons_parse() {
	let input = "<x=-1, y=0, z=2>
<x=2, y=-10, z=7>
<x=3, y=5, z=-1>";
	let expected = Moons { v: vec![
		Moon {
			pos: vec![-1, 0, 2],
			vel: vec![0, 0, 0]
		},
		Moon {
			pos: vec![2, -10, 7],
			vel: vec![0, 0, 0]
		},
		Moon {
			pos: vec![3, 5, -1],
			vel: vec![0, 0, 0]
		},
	]};
	assert_eq!(Moons::parse(input), expected);
}

#[test]
fn test_step() {
	let mut moons = Moons::parse("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>");
	
	let expected = parse_state(vec![
		"pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>",
		"pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>",
		"pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>",
		"pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>"
	]);

	moons.step(0);
	moons.step(1);
	moons.step(2);
	assert_eq!(moons, expected);
}

#[test]
fn test_step_repeated() {
	let mut moons = Moons::parse("<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>");

	let expected = parse_state(vec![
		"pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>",
		"pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>",
		"pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>",
		"pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>",
	]);

	for _ in 0..100 {
		moons.step(0);
		moons.step(1);
		moons.step(2);
	}

	assert_eq!(moons, expected);
}

#[test]
fn test_find_repeat() {
	let mut moons = parse_state(vec![
		"pos=<x= -1, y=  0, z=  2>, vel=<x=  0, y=  0, z=  0>",
		"pos=<x=  2, y=-10, z= -7>, vel=<x=  0, y=  0, z=  0>",
		"pos=<x=  4, y= -8, z=  8>, vel=<x=  0, y=  0, z=  0>",
		"pos=<x=  3, y=  5, z= -1>, vel=<x=  0, y=  0, z=  0>",
	]);

	assert_eq!(find_repeat(&mut moons), 2772);
}

#[test]
fn test_find_repeat_long() {
	let mut moons = Moons::parse("<x=-8, y=-10, z=0>
<x=5, y=5, z=10>
<x=2, y=-7, z=3>
<x=9, y=-8, z=-3>");

	assert_eq!(find_repeat(&mut moons), 4686774924);
}

