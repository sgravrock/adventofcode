mod input;
use std::collections::HashSet;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::fmt;
use regex::Regex;

fn main() {
	let mut moons = Moons::parse(input::puzzle_input());
	let steps = find_repeat(&mut moons);
	println!("The universe repeats after {} steps.", steps);
}

fn find_repeat(moons: &mut Moons) -> u64 {
	// Hash the hash codes rather than the states themselves.
	// This saves huge amounts of memory.
	let mut seen = HashSet::new();
	seen.insert(hash(moons));
	let mut i = 0;

	loop {
		moons.step();
		i += 1;

		if !seen.insert(hash(&moons)) {
			return i;
		}

		if i % 1000000 == 0 {
			println!("{} steps...", i);
		}
	}
}

fn hash(moons: &Moons) -> u64 {
	let mut hasher = DefaultHasher::new();
	moons.hash(&mut hasher);
	hasher.finish()
}

#[derive(PartialEq, Hash, Copy, Clone)]
struct Triplet { x: i32, y: i32, z: i32 }

#[derive(PartialEq, Hash, Copy, Clone)]
struct Moon {
	pos: Triplet,
	vel: Triplet
}

impl fmt::Debug for Moon {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "pos=<x={}, y={}, z={}>, vel=<x={}, y={}, z={}>",
			self.pos.x, self.pos.y, self.pos.z,
			self.vel.x, self.vel.y, self.vel.z
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
					pos: Triplet {x: x, y: y, z: z},
					vel: Triplet { x: 0, y: 0, z: 0}
				}
			})
			.collect();
		
		Moons { v }
	}

	fn step(&mut self) {
		self.apply_gravity();
		self.apply_velocity();
	}

	fn apply_gravity(&mut self) {
		for (i, j) in each_pair(self.v.len()) {
			self.v[i].vel.x +=
				if self.v[i].pos.x < self.v[j].pos.x {
					1
				} else if self.v[i].pos.x > self.v[j].pos.x {
					-1
				} else {
					0
				};
			self.v[i].vel.y +=
				if self.v[i].pos.y < self.v[j].pos.y {
					1
				} else if self.v[i].pos.y > self.v[j].pos.y {
					-1
				} else {
					0
				};
			self.v[i].vel.z +=
				if self.v[i].pos.z < self.v[j].pos.z {
					1
				} else if self.v[i].pos.z > self.v[j].pos.z {
					-1
				} else {
					0
				};
		}
	}
	
	fn apply_velocity(&mut self) {
		for moon in &mut self.v {
			moon.pos.x += moon.vel.x;
			moon.pos.y += moon.vel.y;
			moon.pos.z += moon.vel.z;
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
				pos: Triplet {x: px, y: py, z: pz},
				vel: Triplet { x: vx, y: vy, z: vz}
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
			pos: Triplet { x: 30, y: -8, z: 3 },
			vel: Triplet { x: 3, y: -3, z: 0}
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
			pos: Triplet {x: -1, y: 0, z: 2},
			vel: Triplet {x: 0, y: 0, z: 0}
		},
		Moon {
			pos: Triplet {x: 2, y: -10, z: 7},
			vel: Triplet {x: 0, y: 0, z: 0}
		},
		Moon {
			pos: Triplet {x: 3, y: 5, z: -1},
			vel: Triplet {x: 0, y: 0, z: 0}
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

	moons.step();
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
		moons.step();
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

