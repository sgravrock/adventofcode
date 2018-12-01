func interpret(instructions: [String], initialValues: Dictionary<String, Int>) -> Dictionary<String, Int> {
	var registers = initialValues
	var ip = 0
	
	while ip >= 0 && ip < instructions.count {
		let tokens = instructions[ip]
			.replacingOccurrences(of: ",", with: "")
			.components(separatedBy: " ")
		var offset = 1
		
		switch tokens[0] {
		case "inc":
			registers[tokens[1]]! += 1
		case "hlf":
			registers[tokens[1]]! /= 2
		case "tpl":
			registers[tokens[1]]! *= 3
		case "jmp":
			offset = Int(tokens[1])!
		case "jie":
			if registers[tokens[1]]! % 2 == 0 {
				offset = Int(tokens[2])!
			}
		case "jio":
			if registers[tokens[1]]! == 1 {
				offset = Int(tokens[2])!
			}
		default:
			assertionFailure("Unknown instruction \(tokens[0])")
		}
		
		ip += offset
	}
	
	return registers
}
