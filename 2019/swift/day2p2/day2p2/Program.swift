import Cocoa

let Halt = 99;
let Add = 1;
let Mul = 2;

struct RuntimeError: Error {
    let message: String

    init(_ message: String) {
        self.message = message
    }

    public var localizedDescription: String {
        return message
    }
}

class Program {
	static func parse(_ input: String) -> [Int] {
		return input.split(separator: ",").map({ Int($0)! })
	}
	
	static func findInputs(program: [Int], producingResult desiredResult: Int) throws -> (Int, Int) {
		for i in 0..<program.count {
			for j in 0..<program.count {
				if (try execute(program, noun: i, verb: j) == desiredResult) {
					return (i, j)
				}
			}
		}
		
		return (-1, -1)
	}
	
	static func execute(_ program: [Int], noun: Int, verb: Int) throws -> Int {
		var mem = program
		mem[1] = noun
		mem[2] = verb
		try execute(&mem)
		return mem[0]
	}
	
	static func execute(_ program: inout [Int]) throws {
		var ip = 0
		
		while (program[ip] != Halt) {
			switch (program[ip]) {
			case Add:
				program[program[ip + 3]] = program[program[ip + 1]] + program[program[ip + 2]]
				break
			case Mul:
				program[program[ip + 3]] = program[program[ip + 1]] * program[program[ip + 2]]
				break
			default:
				throw RuntimeError("Unrecognized opcode \(program[ip])")
			}
			ip += 4
		}
	}
}
