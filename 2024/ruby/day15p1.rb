#!/usr/bin/env ruby

BOX = "O"
WALL = "#"
EMPTY = "."
ROBOT = "@"
UP = "^"
DOWN = "v"
LEFT = "<"
RIGHT = ">"

class Grid
	attr_reader :width, :height, :cells

	def initialize(width, height, cells)
		@width = width
		@height = height
		@cells = cells
	end

	def sum_of_box_coordinates
		cells
			.filter { |_, contents| contents == BOX }
			.map { |coord, _| coord[:x] + 100 * coord[:y] }
			.sum
	end
end

class Robot
	attr_reader :pos

	def initialize(x, y)
		@pos = {x: x, y: y}
	end

	def move(input, grid)
		delta = if input == UP
			{x: 0, y: -1}
		elsif input == DOWN
			{x: 0, y: 1}
		elsif input == LEFT
			{x: -1, y: 0}
		elsif input == RIGHT
			{x: 1, y: 0}
		else
			raise "Unexpected input: '#{input}'"
		end

		dest = {x: pos[:x] + delta[:x], y: pos[:y] + delta[:y]}

		can_move = if grid.cells[dest] == BOX
			shove(dest, delta, grid)
		else
			grid.cells[dest] != WALL
		end

		if can_move then
			@pos = dest
		end
	end

	def shove(to_shove, delta, grid)
		dest = {x: to_shove[:x] + delta[:x], y: to_shove[:y] + delta[:y]}

		ok = if grid.cells[dest] == BOX
			shove(dest, delta, grid)
		else
			grid.cells[dest] != WALL
		end

		if ok then
			grid.cells[dest] = BOX
			grid.cells[to_shove] = EMPTY
		end

		ok
	end
end

def parse_input(input)
	chunks = input.split("\n\n")
	moves = chunks[1].chars.reject { |c| c == "\n" }
	grid_lines = chunks[0].split("\n")
	height = grid_lines.length
	width = grid_lines[0].length
	cells = Hash.new
	robot = nil

	grid_lines.each_with_index do |line, y|
		line.chars.each_with_index do |c, x|
			if c == ROBOT
				robot = Robot.new(x, y)
				cells[{x: x, y: y}] = '.'
			else
				cells[{x: x, y: y}] = c
			end
		end
	end

	[Grid.new(width, height, cells), robot, moves]
end

grid, robot, moves = parse_input(STDIN.read)

moves.each do |m|
	robot.move(m, grid)
end

puts grid.sum_of_box_coordinates
