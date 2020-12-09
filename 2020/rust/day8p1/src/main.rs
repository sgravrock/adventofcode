mod input;

fn main() {
    println!("{}", solve(input::puzzle_input()));
	 // 1727
}

#[derive(PartialEq, Eq, Debug)]
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
	let mut acc: i32 = 0;
	let mut ip: usize = 0;
	let mut visited: Vec<bool> = program.iter().map(|_| false).collect();

	loop {
		if visited[ip] {
			return acc;
		}

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
	assert_eq!(solve(input), 5);
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
