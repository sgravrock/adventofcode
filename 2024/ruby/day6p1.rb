#!/usr/bin/env ruby

Coord = Struct.new(:x, :y)
Guard = Struct.new(:coord, :orientation) do
	def advance(grid)
		next_coord = case orientation
		when "^"
			Coord.new(coord.x, coord.y - 1)
		when "v"
			Coord.new(coord.x, coord.y + 1)
		when "<"
			Coord.new(coord.x - 1, coord.y)
		when ">"
			Coord.new(coord.x + 1, coord.y)
		else
			raise "Unexpected orientation: '#{orientation}'"
		end

		if obstacle?(grid, next_coord) then
			turn
			advance(grid)
		else
			if coord == next_coord
				raise "oh no didn't move"
			end

			self.coord = next_coord
		end
	end

	def turn
		self.orientation = case orientation
		when "^"
			">"
		when ">"
			"v"
		when "v"
			"<"
		when "<"
			"^"
		end
	end
end

def solve(grid)
	visited = Set.new
	guard = Guard.new(find("^", grid), "^")

	while in_bounds?(grid, guard.coord)
		visited.add(guard.coord)
		guard.advance(grid)
	end

	visited.count
end

def in_bounds?(grid, coord)
	(0...grid.length).cover?(coord.y) &&
		(0...grid[0].length).cover?(coord.x)
end

def obstacle?(grid, coord)
	if in_bounds?(grid, coord)
		grid[coord.y][coord.x] == '#'
	else
		false
	end
end

def find(needle, haystack)
	haystack.each_with_index do |row, y|
		row.each_with_index do |cell, x|
			if cell == needle
				return Coord.new(x, y)
			end
		end
	end
end

grid = ARGF.read.split("\n").map(&:chars)
p solve(grid)
