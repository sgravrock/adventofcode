use std::collections::HashMap;
use std::collections::hash_map;
use std::env;

fn main() {
	let input = "cpy a b
dec b
cpy a d
cpy 0 a
cpy b c
inc a
dec c
jnz c -2
dec d
jnz d -5
dec b
cpy b c
cpy c d
dec d
inc c
jnz d -2
tgl c
cpy -16 c
jnz 1 c
cpy 90 c
jnz 73 d
inc a
inc d
jnz d -2
inc c
jnz c -5";
	println!("{}", compute(input).get('a'));
}

#[derive(Copy, Clone, Debug)]
enum Rvalue {
	Const(i32),
	Reg(char)
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
	Inc(Rvalue),
	Dec(Rvalue),
	Cpy { src: Rvalue, dest: Rvalue },
	Jnz { criterion: Rvalue, offset: Rvalue },
	Tgl(Rvalue)
}

fn compute(input: &str) -> RegisterFile {
	let mut registers = RegisterFile::new();
	let mut instructions = parse_input(input);
	let mut ip: i32 = 0;
	let debug = match env::var("DEBUG") {
		Ok(val) => val.len() != 0,
		Err(_) => false
	};

	while ip >= 0 && ip < instructions.len() as i32 {
		let instr = instructions[ip as usize].clone();
		ip += 1;

		if debug {
			println!("Executing {:?}", instr);
		}

		match instr {
			Instruction::Inc(op) => {
				if let Rvalue::Reg(reg) = op {
					*(registers.get_mut(reg)) += 1;
				}
			},
			Instruction::Dec(op) => {
				if let Rvalue::Reg(reg) = op {
					*(registers.get_mut(reg)) -= 1;
				}
			},
			Instruction::Cpy { src, dest } => {
				if let Rvalue::Reg(reg) = dest {
					if debug {
						println!("Copying to {:?}", reg);
					}

					*(registers.get_mut(reg)) = expand_rvalue(src, &registers);
				}
			},
			Instruction::Jnz { criterion, offset } => {
				let n = expand_rvalue(criterion, &registers);

				if n != 0 {
					ip += expand_rvalue(offset, &registers) - 1;
				}
			},
			Instruction::Tgl(offset) => {
				let n = expand_rvalue(offset, &registers);
				let i = (ip - 1 + n) as usize;

				if i < instructions.len() {
					instructions[i] = toggle(&instructions[i]);
				}
			}
		}

		if debug {
			print_state(ip, &registers);
		}
	}

	registers
}

fn print_state(ip: i32, registers: &RegisterFile) {
	print!("{} ", ip);

	for (k, v) in registers.iter() {
		print!("{}={} ", k, v);
	}

	println!("");
}

fn toggle(src: &Instruction) -> Instruction {
	let result = match src {
		&Instruction::Inc(n) => Instruction::Dec(n),
		&Instruction::Dec(n) => Instruction::Inc(n),
		&Instruction::Tgl(n) => Instruction::Inc(n),
		&Instruction::Cpy { src, dest } =>
			Instruction::Jnz { criterion: src, offset: dest },
		&Instruction::Jnz { criterion, offset } =>
			Instruction::Cpy { src: criterion, dest: offset }
	};
	println!("Toggling {:?} to {:?}", src, result);
	result
}


#[test]
fn test_compute() {
	let input = "cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a";
	let regs = compute(input);
	assert_eq!(3, regs.get('a'));

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
fn test_jnz_offset_is_reg() {
	let input = "cpy 2 b
jnz 1 b
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

#[test]
fn test_tgl_unary() {
	let input = "tgl 2
tgl 2
inc a
dec b";
	let regs = compute(input);
	assert_eq!(-1, regs.get('a'));
	assert_eq!(1, regs.get('b'));
}

#[test]
fn test_tgl_cpy() {
	let input = "cpy a 1
tgl 1
cpy a 2
inc a
inc a";
	let regs = compute(input);
	assert_eq!(2, regs.get('a'));
}

#[test]
fn test_tgl_tgl() {
	// 2nd tgl becomes a nop
	let input = "tgl 1
tgl 1
dec a";
	let regs = compute(input);
	assert_eq!(-1, regs.get('a'));

	// 2nd tgl becomes an inc
	let input2 = "tgl 1
tgl a";
	let regs2 = compute(input2);
	assert_eq!(1, regs2.get('a'));
}

#[test]
fn test_tgl_jnz() {
	let input = "tgl 1
jnz 42 a";
	let regs = compute(input);
	assert_eq!(42, regs.get('a'));
}

#[test]
fn test_tgl_oob() {
	let input = "tgl 2
inc a";
	let regs = compute(input);
	assert_eq!(1, regs.get('a'));
}

fn expand_rvalue(rvalue: Rvalue, registers: &RegisterFile) -> i32 {
	match rvalue {
		Rvalue::Const(n) => n,
		Rvalue::Reg(r) => registers.get(r)
	}
}

fn parse_input(input: &str) -> Vec<Instruction> {
	input.lines()
		.map(|s| {
			let ts: Vec<&str> = s.split(' ').collect();

			match ts[0] {
				"inc" => Instruction::Inc(parse_rvalue(ts[1])),
				"dec" => Instruction::Dec(parse_rvalue(ts[1])),
				"cpy" => Instruction::Cpy {
					src: parse_rvalue(ts[1]),
					dest: parse_rvalue(ts[2])
				},
				"jnz" => Instruction::Jnz {
					criterion: parse_rvalue(ts[1]),
					offset: parse_rvalue(ts[2])
				},
				"tgl" => Instruction::Tgl(parse_rvalue(ts[1])),
				_ => panic!("Can't decode: {}", s)
			}
		})
		.collect()
}

fn first_char(s: &str) -> char {
	s.chars().next().unwrap()
}

fn parse_rvalue(s: &str) -> Rvalue {
	match s.parse::<i32>() {
		Ok(k) => Rvalue::Const(k),
		Err(_) => Rvalue::Reg(first_char(s))
	}
}


struct RegisterFile {
	registers: HashMap<char, i32>
}

impl RegisterFile {
	fn new() -> RegisterFile {
		let mut registers: HashMap<char, i32> = HashMap::new();
		registers.insert('c', 1);

		for x in ['a', 'b', 'd'].iter() {
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

	fn iter(&self) -> hash_map::Iter<char, i32> {
		self.registers.iter()
	}
}
