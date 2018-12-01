import Foundation

func parse_input(_ lines: [String]) -> [[Int]] {
	let unreordered = lines.map({ (line: String) -> [Int] in
		return line.components(separatedBy: " ")
			.filter({ $0 != "" })
			.map({ Int($0)! })
	})
	
	var result: [[Int]] = []
	
	for i in 0..<unreordered.count / 3 {
		let base = i * 3;
		
		for j in 0..<3 {
			result.append([
				unreordered[base][j],
				unreordered[base + 1][j],
				unreordered[base + 2][j],
			])
		}
	}
	
	return result
}

func is_triangle(_ edges: [Int]) -> Bool {
	return edges[0] + edges[1] > edges[2] &&
		edges[1] + edges[2] > edges[0] &&
		edges[0] + edges[2] > edges[1]
}
