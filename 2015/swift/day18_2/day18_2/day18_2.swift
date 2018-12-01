func parseLayout(_ lines: [String]) -> [[Bool]] {
	return lines.map({ (line: String) -> [Bool] in
		return line.characters.map { $0 == "#" }
	})
}

func numLightsOn(grid: [[Bool]], steps: Int) -> Int {
	var currentGrid = grid
	
	for _ in 1...steps {
		currentGrid = iterate(currentGrid)
	}
	
	return countOn(currentGrid)
}

func countOn(_ grid: [[Bool]]) -> Int {
	var n = 0
	
	for row in grid {
		for cell in row {
			if cell {
				n += 1
			}
		}
	}
	
	return n
}

func iterate(_ input: [[Bool]]) -> [[Bool]] {
	return (0..<input.count).map({ (x: Int) -> [Bool] in
		return (0..<input[x].count).map({ (y: Int) -> Bool in
			return nextState(input, x: x, y: y)
		})
	})
}

func nextState(_ grid: [[Bool]], x: Int, y: Int) -> Bool {
	if isCorner(grid, x: x, y: y) {
		return true
	}
	
	let n = neighborsOn(grid, x: x, y: y)
	return n == 3 || (n == 2 && grid[x][y])
}

func isCorner(_ grid: [[Bool]], x: Int, y: Int) -> Bool {
	return (x == 0 || x == grid.count - 1) && (y == 0 || y == grid[0].count - 1)
}

func neighborsOn(_ grid: [[Bool]], x: Int, y: Int) -> Int {
	var n = 0
	
	for nx in [-1, 1] {
		if x + nx >= 0 && x + nx < grid.count {
			for ny in [-1, 1] {
				if y + ny >= 0 && y + ny < grid[x].count {
					if grid[x + nx][y + ny] {
						n += 1
					}
				}
			}
		}
	}
	
	if x > 0 && grid[x - 1][y] {
		n += 1
	}
	if x < grid.count - 1 && grid[x + 1][y] {
		n += 1
	}
	if y > 0 && grid[x][y - 1] {
		n += 1
	}
	if y < grid[x].count - 1 && grid[x][y + 1] {
		n += 1
	}
	
	return n
}
