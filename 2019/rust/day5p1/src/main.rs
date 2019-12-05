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

fn execute(program: &mut Vec<usize>, input: Option<usize>) -> Vec<usize> {
	let mut ip: usize = 0;
	let mut output: Vec<usize> = vec![];

	while program[ip] != 99 {
		let opcode = program[ip];

		if opcode == 1  || opcode == 2 {
			let arg0 = program[program[ip + 1]];
			let arg1 = program[program[ip + 2]];
			let dest = program[ip + 3];
	
			if opcode == 1 {
				program[dest] = arg0 + arg1;
			} else {
				program[dest] = arg0 * arg1;
			}
	
			ip += 4;
		} else if opcode == 3 {
			let dest = program[ip + 1];
			program[dest] = input.unwrap();
			ip += 2;
		} else if opcode == 4 {
			output.push(program[program[ip + 1]]);
			ip += 2;
		} else {
			panic!("Unrecognized opcode {} at ip={}", program[ip], ip);
		}

	}

	output
}

#[test]
fn test_execute() {
	let mut program = vec![1,9,10,3,2,3,11,0,99,30,40,50];
	let expected = vec![3500,9,10,70,2,3,11,0,99,30,40,50];
	execute(&mut program, None);
	assert_eq!(program, expected);
}

#[test]
fn test_execute_input() {
	let mut program = vec![3,4,99,0,0];
	let expected = vec![3,4,99,0,5];
	execute(&mut program, Some(5));
	assert_eq!(program, expected);
}

#[test]
fn test_execute_output() {
	let mut program = vec![4,3,99,12345];
	assert_eq!(execute(&mut program, None), vec![12345]);
}
