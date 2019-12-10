use crate::machine::Machine;
use std::io;
use std::io::Write;
use std::cmp::min;

pub fn debug(mut machine: &mut Machine) {
	println!("{} words of memory", machine.mem.len());
	show_machine_state(&machine);

	loop {
		print!("debugger> ");
		io::stdout().flush().unwrap();
		let line: String = read!("{}\n");
		let tokens: Vec<&str> = line.split(" ").collect();

		match tokens[0] {
			"q" | "quit" => return,
			"mem" => {
				dump_mem(tokens, &machine);
			},
			"s" | "step" => {
				debug_step(&mut machine);
			},
			"r" | "run" => {
				while machine.mem[machine.ip as usize] != 99 &&
						debug_step(&mut machine) {
				}
			},
			"o" | "output" => {
				println!("{:?}", machine.output.dequeue());
			},
			_ => {
				println!("Commands:");
				println!("mem [start end]: dump memory");
				println!("o, output        read a word from output");
				println!("s, step:         perform the instruction at ip");
				println!("r, run:          run from current position");
				println!("q, quit:         exit");
			}
		}
	}
}

fn debug_step(machine: &mut Machine) -> bool {
	let ok = match machine.do_current_instruction() {
		Ok(_) => true,
		Err(e) => {
			println!("{:#?}", e);
			false
		}
	};

	if machine.mem[machine.ip as usize] != 99 {
		show_machine_state(&machine);
	}

	ok
}

fn show_machine_state(machine: &Machine) {
	println!("ip={} rb={}", machine.ip, machine.relative_base);
	let ip = machine.ip as usize;
	println!("mem[ip..ip+3]={:?}", &machine.mem[ip..min(ip+4, machine.mem.len() - 1)]);
	println!("instruction at ip: {:?}", machine.current_instruction());
}

fn dump_mem(cmd: Vec<&str>, machine: &Machine) {
	match cmd.len() {
		1 => println!("{:?}", machine.mem),
		3 => {
			match cmd[1].parse::<usize>() {
				Ok(start) => {
					match cmd[2].parse::<usize>() {
						Ok(end) => {
							if start < end {
								println!("{:?}", &machine.mem[start..end+1]);
							} else {
								println!("Start must be before end");
							}
						},
						Err(_) => println!("End must be a nonnegative int")
					}
				},
				Err(_) => println!("Start must be a nonnegative int")
			}
		},
		_ => println!("Usage: mem [start end]")
	};
}
