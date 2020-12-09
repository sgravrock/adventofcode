mod input;

fn main() {
    println!("{}", solve(input::puzzle_input()));
	 // 552
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct Instruction {
	opcode: String,
	operand: i32,
}

impl Instruction {
	fn new(opcode: &str, operand: i32) -> Instruction {
		Instruction { opcode: opcode.to_string(), operand }
	}
}

fn solve(input: &str) -> i32 {
	let program = parse(input);

	for ip in 0..program.len() {
		let opcode = &program[ip].opcode;

		if opcode != "acc" {
			let mut updated = program.clone();
			updated[ip].opcode = if opcode == "jmp" {
				"nop"
			} else {
				"jmp"
			}.to_string();
			let (halted, acc) = halts(&updated);

			if halted {
				return acc;
			}
		}
	}

	panic!("No variants halted");
}

#[test]
fn test_solve() {
	let input = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";
	assert_eq!(solve(input), 8);
}

fn halts(program: &Vec<Instruction>) -> (bool, i32) {
	let mut acc: i32 = 0;
	let mut ip: usize = 0;
	let mut visited: Vec<bool> = program.iter().map(|_| false).collect();

	while ip < visited.len() && !visited[ip] {
		visited[ip] = true;
		let mut delta: i32 = 1;

		if program[ip].opcode == "nop" {
			// do nothing
		} else if program[ip].opcode == "acc" {
			 acc += program[ip].operand;
		} else if program[ip].opcode == "jmp" {
			 delta = program[ip].operand;
		} else {
			panic!("Unknown opcode: {}", program[ip].opcode);
		}

		ip = ((ip as i32) + delta) as usize;
	}

	(ip == visited.len(), acc)
}

#[test]
fn test_halts_false() {
	let program = parse("nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6");
	assert_eq!(halts(&program).0, false);
}

#[test]
fn test_halts_true() {
	let program = parse("nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6");
	assert_eq!(halts(&program), (true, 8));
}

fn parse(input: &str) -> Vec<Instruction> {
	input.lines()
		.map(|line| {
			let mut fields = line.split(" ");
			Instruction::new(
				fields.next().unwrap(),
				fields.next().unwrap().parse().unwrap()
			)
		})
		.collect()
}

#[test]
fn test_parse() {
	let input = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6";
	let expected = vec![
		Instruction::new("nop", 0 ),
		Instruction::new("acc", 1 ),
		Instruction::new("jmp", 4 ),
		Instruction::new("acc", 3 ),
		Instruction::new("jmp", -3 ),
		Instruction::new("acc", -99 ),
		Instruction::new("acc", 1 ),
		Instruction::new("jmp", -4 ),
		Instruction::new("acc", 6 ),
	];
	assert_eq!(parse(input), expected);
}
