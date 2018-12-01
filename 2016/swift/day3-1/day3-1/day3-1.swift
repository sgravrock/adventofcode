import Foundation

func parse_input(_ lines: [String]) -> [[Int]] {
	return lines.map({ (line: String) -> [Int] in
		return line.components(separatedBy: " ")
			.filter({ $0 != "" })
			.map({ Int($0)! })
	})
}

func is_triangle(_ edges: [Int]) -> Bool {
	return edges[0] + edges[1] > edges[2] &&
		edges[1] + edges[2] > edges[0] &&
		edges[0] + edges[2] > edges[1]
}
