use std::collections::HashMap;

fn main() {
}

#[derive(Copy, Clone, Debug)]
enum Rvalue {
	Const(i32),
	Reg(char)
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
	Inc(char),
	Dec(char),
	Cpy(char, Rvalue),
	Jnz(Rvalue, i32),
	Tgl(Rvalue)
}

fn compute(input: &str) -> RegisterFile {
	let mut registers = RegisterFile::new();
	let mut instructions = parse_input(input);
	let mut ip: i32 = 0;

	while ip >= 0 && ip < instructions.len() as i32 {
		let instr = instructions[ip as usize].clone();
		ip += 1;

		match instr {
			Instruction::Inc(reg) => *(registers.get_mut(reg)) += 1,
			Instruction::Dec(reg) => *(registers.get_mut(reg)) -= 1,
			Instruction::Cpy(reg, rvalue) => 
				*(registers.get_mut(reg)) = expand_rvalue(rvalue, &registers),
			Instruction::Jnz(rvalue, offset) => {
				let n = expand_rvalue(rvalue, &registers);

				if n != 0 {
					ip += offset - 1;
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
	}

	registers
}

fn toggle(src: &Instruction) -> Instruction {
	match src {
		&Instruction::Inc(n) => Instruction::Dec(n),
		&Instruction::Dec(n) => Instruction::Inc(n),
		&Instruction::Tgl(n) => Instruction::Tgl(n), // TODO
		_ => panic!("Don't know how to toggle {:?}", src)
	}
}


/* TODO: broken until all the variations of tgl are implemented
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
*/

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

/* TODO: Need to be able to store invalid instructions (like cpy 1 a) first
#[test]
fn test_tgl_binary() {
	let input = "cpy a 1
tgl 1
cpy a 2
inc a
inc a";
	let regs = compute(input);
	assert_eq!(2, regs.get('a'));
}
*/

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
				"inc" => Instruction::Inc(first_char(ts[1])),
				"dec" => Instruction::Dec(first_char(ts[1])),
				"cpy" => Instruction::Cpy(first_char(ts[2]), parse_rvalue(ts[1])),
				"jnz" => Instruction::Jnz(parse_rvalue(ts[1]),
					ts[2].parse::<i32>().unwrap()),
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
}
