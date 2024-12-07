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
			turn().advance(grid)
		else
			if coord == next_coord
				raise "oh no didn't move"
			end

			Guard.new(next_coord, orientation)
		end
	end

	def turn
		next_orientation = case orientation
		when "^"
			">"
		when ">"
			"v"
		when "v"
			"<"
		when "<"
			"^"
		end

		Guard.new(coord, next_orientation)
	end
end

def loops?(grid)
	visited = Set.new
	guard = Guard.new(find("^", grid), "^")

	while in_bounds?(grid, guard.coord)
		if visited.include?(guard)
			return true
		end

		visited.add(guard)
		guard = guard.advance(grid)
	end

	return false
end

def solve(grid)
	n_found = 0

	(0...grid.length).each do |y|
		(0...grid[y].length).each do |x|
			if grid[y][x] == "." then
				grid[y][x] = "#"

				if loops?(grid) then
					n_found += 1
				end

				grid[y][x] = "."
			end
		end
	end


	n_found
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
