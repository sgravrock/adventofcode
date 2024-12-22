#!/usr/bin/env ruby

def make_grid(size)
	grid = Hash.new

	size.times.each do |y|
		size.times.each do |x|
			grid[{x: x, y: y}] = :ok
		end
	end

	grid
end


def solve(grid_size, input)
	origin = {x: 0, y: 0}
	goal = {x: grid_size-1, y: grid_size-1}
	i = 0
	grid = make_grid(grid_size)

	while true
		grid[input[i]] = :corrupted

		unless reachable?(grid, origin, goal)
			return "#{input[i][:x]},#{input[i][:y]}"
			return input[i]
		end

		i += 1
	end
end

def reachable?(grid, origin, goal)
	costs = Hash.new
	costs[origin] = 0
	frontier = Set.new([origin])
	directions = [
		{x: 0, y: 1},
		{x: 0, y: -1},
		{x: 1, y: 0},
		{x: -1, y: 0},
	]

	while not frontier.empty? do
		pos = frontier.first
		frontier.delete(pos)

		if pos == goal
			return true
		end

		directions.each do |dir|
			neighbor = {x: pos[:x] + dir[:x], y: pos[:y] + dir[:y]}

			if grid.include?(neighbor) &&
					grid[neighbor] == :ok &&
					!costs.include?(neighbor)
				frontier.add(neighbor)
				costs[neighbor] = costs[pos] + 1
			end
		end
	end

	false
end

if ARGV.length != 1
	puts "Usage: #{$0} grid-size"
	exit(1)
end

grid_size = ARGV[0].to_i
input = STDIN.read.split("\n")
	.map { |line|
		fields = line.split(",")
		{ x: fields[0].to_i, y: fields[1].to_i }
	}
puts(solve(grid_size, input))
