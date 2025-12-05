#!/usr/bin/env ruby

require './microtest'

Coord = Data.define(:x, :y) do
	def neighbors
		deltas = [-1, 0, 1]
		deltas
			.flat_map { |dx|
				deltas.map { |dy| [dx, dy] }
			}
			.reject { |dx, dy| dx == 0 && dy == 0 }
			.map { |dx, dy| Coord.new(x + dx, y + dy) }
	end
end

class Grid
	attr_reader :width, :height

	def initialize(input)
		rows = input.split("\n")
		@height = rows.length
		@width = rows[0].length

		@cells = Hash.new
		rows.each_with_index do |row, y|
			row.chars.each_with_index do |c, x|
				@cells[Coord.new(x, y)] = c
			end
		end
	end

	# Out of range access returns nil
	def [](coord)
		@cells[coord]
	end


	def each_coord
		(0...height).each do |y|
			(0...width).each do |x|
				yield Coord.new(x, y)
			end
		end
	end
end


def find_accessible(grid)
	result = Set.new
	grid.each_coord do |coord|
		if grid[coord] == '@' && occupied_neighbors(grid, coord) < 4 then
			result.add(coord)
		end
	end
	result
end

def occupied_neighbors(grid, coord)
	coord.neighbors
		.select { |n| grid[n] == '@' }
		.count
end


class Tests < Microtest::Test
	def test_coord_neighbors
		subject = Coord.new(5, 7)
		expected = Set.new([
			Coord.new(4, 6), Coord.new(5, 6), Coord.new(6, 6),
			Coord.new(4, 7),                  Coord.new(6, 7),
			Coord.new(4, 8), Coord.new(5, 8), Coord.new(6, 8)
		])
		assert_equal(expected, Set.new(subject.neighbors))
	end

	def test_find_accessible
		input = <<~END
			..@@.@@@@.
			@@@.@.@.@@
			@@@@@.@.@@
			@.@@@@..@.
			@@.@@@@.@@
			.@@@@@@@.@
			.@.@.@.@@@
			@.@@@.@@@@
			.@@@@@@@@.
			@.@.@@@.@.
		END
		grid = Grid.new(input)
		expected = Set.new([
			Coord.new(2, 0),
			Coord.new(3, 0),
			Coord.new(5, 0),
			Coord.new(6, 0),
			Coord.new(8, 0),
			Coord.new(0, 1),
			Coord.new(6, 2),
			Coord.new(0, 4),
			Coord.new(9, 4),
			Coord.new(0, 7),
			Coord.new(0, 9),
			Coord.new(2, 9),
			Coord.new(8, 9),
		])
		assert_equal(expected, find_accessible(grid))
	end
end


Microtest.run(Tests.new)
g = Grid.new(ARGF.read)
puts(find_accessible(g).size)
