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

#[derive(Clone, Debug)]
enum Rvalue {
	Const(i32),
	Reg(char)
}

#[derive(Clone, Debug)]
enum Instruction {
	Inc(char),
	Dec(char),
	Cpy(char, Rvalue),
	Jnz(Rvalue, i32)
}

fn compute(input: &str) -> RegisterFile {
	let mut registers = RegisterFile::new();
	let instructions = parse_input(input);
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
			}
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
