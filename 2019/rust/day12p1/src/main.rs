mod input;
use std::fmt;
use regex::Regex;

fn main() {
	let mut moons = parse_input(input::puzzle_input());

	for _ in 0..1000 {
		step(&mut moons)
	}

	println!("{}", energy(&moons));
}

#[derive(PartialEq, Copy, Clone)]
struct Position { x: i32, y: i32, z: i32 }
#[derive(PartialEq, Copy, Clone)]
struct Velocity { x: i32, y: i32, z: i32 }

#[derive(PartialEq, Copy, Clone)]
struct Moon {
	pos: Position,
	vel: Velocity
}

impl fmt::Debug for Moon {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "pos=<x={}, y={}, z={}>, vel=<x={}, y={}, z={}>",
			self.pos.x, self.pos.y, self.pos.z,
			self.vel.x, self.vel.y, self.vel.z
		)
	}
}

// Wrap a Moons just to be able to get nicer test failure output.
#[derive(PartialEq)]
struct Moons {
	v: Vec<Moon>
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

fn parse_input(input: &str) -> Moons {
	let re = Regex::new("<x=([0-9\\-]+), y=([0-9\\-]+), z=([0-9\\-]+)>")
		.unwrap();

	let v = input.lines()
		.map(|line| {
			let caps = re.captures(line).unwrap();
			let x = caps.get(1).unwrap().as_str().parse::<i32>().unwrap();
			let y = caps.get(2).unwrap().as_str().parse::<i32>().unwrap();
			let z = caps.get(3).unwrap().as_str().parse::<i32>().unwrap();
			Moon { pos: Position {x: x, y: y, z: z}, vel: Velocity { x: 0, y: 0, z: 0} }
		})
		.collect();
	
	Moons { v }
}

fn step(mut moons: &mut Moons) {
	apply_gravity(&mut moons);
	apply_velocity(&mut moons);
}

fn apply_gravity(moons: &mut Moons) {
	for (i, j) in each_pair(moons.v.len()) {
		moons.v[i].vel.x +=
			if moons.v[i].pos.x < moons.v[j].pos.x {
				1
			} else if moons.v[i].pos.x > moons.v[j].pos.x {
				-1
			} else {
				0
			};
		moons.v[i].vel.y +=
			if moons.v[i].pos.y < moons.v[j].pos.y {
				1
			} else if moons.v[i].pos.y > moons.v[j].pos.y {
				-1
			} else {
				0
			};
		moons.v[i].vel.z +=
			if moons.v[i].pos.z < moons.v[j].pos.z {
				1
			} else if moons.v[i].pos.z > moons.v[j].pos.z {
				-1
			} else {
				0
			};
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

fn apply_velocity(moons: &mut Moons) {
	for moon in &mut moons.v {
		moon.pos.x += moon.vel.x;
		moon.pos.y += moon.vel.y;
		moon.pos.z += moon.vel.z;
	}
}

fn energy(moons: &Moons) -> i32 {
	moons.v.iter()
		.map(|moon| {
			let potential = moon.pos.x.abs() + moon.pos.y.abs() + moon.pos.z.abs();
			let kinetic = moon.vel.x.abs() + moon.vel.y.abs() + moon.vel.z.abs();
			potential * kinetic
		})
		.sum()
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
				pos: Position {x: px, y: py, z: pz},
				vel: Velocity { x: vx, y: vy, z: vz}
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
			pos: Position { x: 30, y: -8, z: 3 },
			vel: Velocity { x: 3, y: -3, z: 0}
		}
	]};
	assert_eq!(parse_state(input), expected);
}


#[test]
fn test_parse_input() {
	let input = "<x=-1, y=0, z=2>
<x=2, y=-10, z=7>
<x=3, y=5, z=-1>";
	let expected = Moons { v: vec![
		Moon {
			pos: Position {x: -1, y: 0, z: 2},
			vel: Velocity {x: 0, y: 0, z: 0}
		},
		Moon {
			pos: Position {x: 2, y: -10, z: 7},
			vel: Velocity {x: 0, y: 0, z: 0}
		},
		Moon {
			pos: Position {x: 3, y: 5, z: -1},
			vel: Velocity {x: 0, y: 0, z: 0}
		},
	]};
	assert_eq!(parse_input(input), expected);
}

#[test]
fn test_step() {
	let mut moons = parse_input("<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>");
	
	let expected = parse_state(vec![
		"pos=<x= 2, y=-1, z= 1>, vel=<x= 3, y=-1, z=-1>",
		"pos=<x= 3, y=-7, z=-4>, vel=<x= 1, y= 3, z= 3>",
		"pos=<x= 1, y=-7, z= 5>, vel=<x=-3, y= 1, z=-3>",
		"pos=<x= 2, y= 2, z= 0>, vel=<x=-1, y=-3, z= 1>"
	]);

	step(&mut moons);
	assert_eq!(moons, expected);
}

#[test]
fn test_step_repeated() {
	let mut moons = parse_input("<x=-8, y=-10, z=0>
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
		step(&mut moons);
	}

	assert_eq!(moons, expected);
}

#[test]
fn test_energy() {
	let moons = parse_state(vec![
		"pos=<x=  8, y=-12, z= -9>, vel=<x= -7, y=  3, z=  0>",
		"pos=<x= 13, y= 16, z= -3>, vel=<x=  3, y=-11, z= -5>",
		"pos=<x=-29, y=-11, z= -1>, vel=<x= -3, y=  7, z=  4>",
		"pos=<x= 16, y=-13, z= 23>, vel=<x=  7, y=  1, z=  1>",
	]);

	assert_eq!(energy(&moons), 1940);
}
