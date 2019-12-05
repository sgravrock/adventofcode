fn main() {
	unimplemented!();
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Inputs {
	noun: i32,
	verb: i32,
}

fn parse_program(input: &str) -> Vec<i32> {
	input
		.split(",")
		.map({|x| x.parse::<i32>().unwrap()})
		.collect()
}

#[test]
fn test_parse_program() {
	assert_eq!(parse_program("5,0,99,2,14"), vec![5,0,99,2,14]);
}

fn execute(program: &mut Vec<i32>, input: Option<i32>) -> Vec<i32> {
	let mut ip = 0;
	let mut output: Vec<i32> = vec![];

	while program[ip] != 99 {
		let opcode = program[ip];

		if opcode == 1  || opcode == 2 {
			let arg0 = program[program[ip as usize + 1] as usize];
			let arg1 = program[program[ip as usize + 2] as usize];
			let dest = program[ip + 3] as usize;
	
			if opcode == 1 {
				program[dest] = arg0 + arg1;
			} else {
				program[dest] = arg0 * arg1;
			}
	
			ip += 4;
		} else if opcode == 3 {
			let dest = program[ip + 1] as usize;
			program[dest] = input.unwrap();
			ip += 2;
		} else if opcode == 4 {
			output.push(program[program[ip + 1] as usize]);
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
fn test_execute_negative() {
	let mut program = vec![1,5,6,7,99,-5,3,0];
	let expected = vec![1,5,6,7,99,-5,3,-2];
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
