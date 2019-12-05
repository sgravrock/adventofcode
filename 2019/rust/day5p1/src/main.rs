fn main() {
	let program = parse_program(
		"1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,13,23,1,23,10,27,1,13,27,31,2,31,10,35,1,35,9,39,1,39,13,43,1,13,43,47,1,47,13,51,1,13,51,55,1,5,55,59,2,10,59,63,1,9,63,67,1,6,67,71,2,71,13,75,2,75,13,79,1,79,9,83,2,83,10,87,1,9,87,91,1,6,91,95,1,95,10,99,1,99,13,103,1,13,103,107,2,13,107,111,1,111,9,115,2,115,10,119,1,119,5,123,1,123,2,127,1,127,5,0,99,2,14,0,0"
	);
	match find_inputs(program, 19690720) {
		None => println!("Not found"),
		Some(result) => println!("{:#?} => {}", result, result.noun * 100 + result.verb)
	};
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

fn find_inputs(program: Vec<usize>, desired_result: usize) -> Option<Inputs> {
	let len = program.len() as usize;

	for noun in 0..len {
		for verb in 0..len {
			let inputs = Inputs {noun, verb};
			if execute_with_inputs(program.clone(), inputs) == desired_result {
				return Some(inputs);
			}
		}
	}

	None
}

#[test]
fn test_find_inputs() {
	let program = vec![1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,13,23,1,23,10,27,1,13,27,31,2,31,10,35,1,35,9,39,1,39,13,43,1,13,43,47,1,47,13,51,1,13,51,55,1,5,55,59,2,10,59,63,1,9,63,67,1,6,67,71,2,71,13,75,2,75,13,79,1,79,9,83,2,83,10,87,1,9,87,91,1,6,91,95,1,95,10,99,1,99,13,103,1,13,103,107,2,13,107,111,1,111,9,115,2,115,10,119,1,119,5,123,1,123,2,127,1,127,5,0,99,2,14,0,0];
	let result = find_inputs(program, 4330636);
	assert_eq!(Some(Inputs {noun: 12, verb: 2}), result)
}

fn execute_with_inputs(mut program: Vec<usize>, inputs: Inputs) -> usize {
	program[1] = inputs.noun;
	program[2] = inputs.verb;
	execute(&mut program);
	program[0]
}

#[test]
fn test_execute_with_inputs() {
	let program = vec![1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,13,23,1,23,10,27,1,13,27,31,2,31,10,35,1,35,9,39,1,39,13,43,1,13,43,47,1,47,13,51,1,13,51,55,1,5,55,59,2,10,59,63,1,9,63,67,1,6,67,71,2,71,13,75,2,75,13,79,1,79,9,83,2,83,10,87,1,9,87,91,1,6,91,95,1,95,10,99,1,99,13,103,1,13,103,107,2,13,107,111,1,111,9,115,2,115,10,119,1,119,5,123,1,123,2,127,1,127,5,0,99,2,14,0,0];
	assert_eq!(
		execute_with_inputs(program, Inputs {noun: 12, verb: 2}),
		4330636
	);
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
