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

def print_grid(grid, size)
	size.times.each do |y|
		size.times.each do |x|
			if grid[{x: x, y: y}] == :corrupted
				print '#'
			else
				print '.'
			end
		end

		puts
	end
end


def drop(grid, input, times)
	i = 0

	while i < input.length && i < times do
		grid[input[i]] = :corrupted
		i += 1
	end
end

def shortest_path(grid, origin, goal)
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
			return costs[pos]
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

	raise "Oh no, never got there"
end

if ARGV.length != 2
	puts "Usage: #{$0} grid-size num-falling-bytes"
	exit(1)
end

grid_size = ARGV[0].to_i
num_falling = ARGV[1].to_i
input = STDIN.read.split("\n")
	.map { |line|
		fields = line.split(",")
		{ x: fields[0].to_i, y: fields[1].to_i }
	}
grid = make_grid(grid_size)
drop(grid, input, num_falling)
print_grid(grid, grid_size)
puts(shortest_path(grid, {x: 0, y: 0}, {x: grid_size-1, y: grid_size-1}))
