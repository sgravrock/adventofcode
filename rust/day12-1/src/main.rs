use std::collections::HashMap;

fn main() {
	let regs = compute("cpy 1 a
cpy 1 b
cpy 26 d
jnz c 2
jnz 1 5
cpy 7 c
inc d
dec c
jnz c -2
cpy a c
inc a
dec b
jnz b -2
cpy c b
dec d
jnz d -6
cpy 16 c
cpy 12 d
inc a
dec d
jnz d -2
dec c
jnz c -5");
	println!("{}", regs.get('a'));
}

fn compute(input: &str) -> RegisterFile {
	let mut registers = RegisterFile::new();
	let instructions = parse_input(input);
	let mut ip: i32 = 0;

	while ip >= 0 && ip < instructions.len() as i32 {
		let instr = instructions[ip as usize].clone();
		ip += 1;

		if instr.0 == "inc" {
			let k = instr.1.chars().next().unwrap();
			let r = registers.get_mut(k);
			*r += 1;
		} else if instr.0 == "dec" {
			let k = instr.1.chars().next().unwrap();
			let r = registers.get_mut(k);
			*r -= 1;
		} else if instr.0 == "cpy" {
			let k = instr.2.unwrap().chars().next().unwrap();
			let v = register_or_const(instr.1, &registers);
			let r = registers.get_mut(k);
			*r = v;
		} else if instr.0 == "jnz" {
			let n = register_or_const(instr.1, &registers);

			if n != 0 {
				let off = instr.2.unwrap().parse::<i32>().unwrap();
				ip += off - 1;
			}
		} else {
			panic!("Don't know how to {}", instr.0);
		}
	}
	registers
}


#[test]
fn test_compute() {
	let input = "cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a";
	let regs = compute(input);
	assert_eq!(42, regs.get('a'));

}

#[test]
fn test_jnz_reg_nope() {
	let input = "jnz a 2
inc a
inc a";
	let regs = compute(input);
	assert_eq!(2, regs.get('a'));
}

#[test]
fn test_jnz_const_nope() {
	let input = "jnz 0 2
inc a
inc a";
	let regs = compute(input);
	assert_eq!(2, regs.get('a'));
}

#[test]
fn test_jnz_const_yes() {
	let input = "jnz 1 2
inc a
inc a";
	let regs = compute(input);
	assert_eq!(1, regs.get('a'));
}

#[test]
fn test_cpy_reg() {
	let input = "cpy 42 a
cpy a b";
	let regs = compute(input);
	assert_eq!(42, regs.get('b'));
}

fn register_or_const(token: String, registers: &RegisterFile) -> i32 {
	match token.parse::<i32>() {
		Ok(k) => k,
		Err(_) => registers.get(token.chars().next().unwrap())
	}
}

fn parse_input(input: &str) -> Vec<(String, String, Option<String>)> {
	input.lines()
		.map(|s| {
			let ts: Vec<&str> = s.split(' ').collect();
			let third = if ts.len() > 2 {
								Some(ts[2].to_string())
							} else {
								None
							};
			(ts[0].to_string(), ts[1].to_string(), third)
		})
		.collect()
}

struct RegisterFile {
	registers: HashMap<char, i32>
}

impl RegisterFile {
	fn new() -> RegisterFile {
		let mut registers: HashMap<char, i32> = HashMap::new();

		for x in ['a', 'b', 'c', 'd'].iter() {
			registers.insert(*x, 0);
		}
		
		RegisterFile { registers: registers }
	}

	fn get(&self, reg: char) -> i32 {
		let r = self.registers.get(&reg)
			.unwrap_or_else(|| panic!("No such register {}", reg));
		*r
	}

	fn get_mut(&mut self, reg: char) -> &mut i32 {
		self.registers.get_mut(&reg)
			.unwrap_or_else(|| panic!("No such register {}", reg))
	}
}
