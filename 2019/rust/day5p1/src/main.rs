fn main() {
	unimplemented!();
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Inputs {
	noun: usize,
	verb: usize,
}

fn parse_program(input: &str) -> Vec<usize> {
	input
		.split(",")
		.map({|x| x.parse::<usize>().unwrap()})
		.collect()
}

#[test]
fn test_parse_program() {
	assert_eq!(parse_program("5,0,99,2,14"), vec![5,0,99,2,14]);
}

fn execute(program: &mut Vec<usize>) {
	let mut ip: usize = 0;

	while program[ip] != 99 {
		let opcode = program[ip];

		if opcode != 1 && opcode != 2 {
			panic!("Unrecognized opcode {} at ip={}", program[ip], ip);
		}

		let arg0 = program[program[ip + 1]];
		let arg1 = program[program[ip + 2]];
		let dest = program[ip + 3];

		if opcode == 1 {
			program[dest] = arg0 + arg1;
		} else {
			program[dest] = arg0 * arg1;
		}

		ip += 4;
	}
}

#[test]
fn test_execute() {
	let mut program = vec![1,9,10,3,2,3,11,0,99,30,40,50];
	let expected = vec![3500,9,10,70,2,3,11,0,99,30,40,50];
	execute(&mut program);
	assert_eq!(program, expected);
}
