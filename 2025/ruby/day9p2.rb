#!/usr/bin/env ruby
require './microtest'

# Because corners are connected in a loop and there are no duplicates, the
# red and green tiles are guaranteed to form a single contiguous region. Based
# on visualzations that various people have provided, puzzle inputs (unlike the
# sample input) form a rough diamond shape that is nearly split by one or more
# horizontal channels near the center. My puzzle input contains 496 red tiles
# (corners) for 245,520 possible rectangles with a width of 96,718, height 
# of 96,714, and total area of 9,353,984,652.
#
# Approaches that don't work:
# * Flood fill to find all green cells, then check each coordinate in each
#   rectangle. Far too slow, uses way too much memory.
# * Naive pixel-by-pixel ray tracing to determine whether each row in each
#   rectangle is entirely inside. Far too slow even for a single rectangle, and
#   would need to either be memoized (using way too much memory) or repeated for
#   each rectangle.
#
# What worked:
# 1. Find all edges, i.e. red cells and cells between pairs of red cells that 
#    are adjacent in the input list.
# 2. Model the grid as an array of rows, with each row represented as an array
#    of contiguous ranges that are inside. Those ranges can be determined from
#    the vertical edges in each row. This allows for checking whether a point is
#    inside in linear time with respect to the numuber of contiguous ranges
#    rather than with repsect to the width of the row. For my puzzle input that
#    works out to a constant factor reduction of about 16,120x for a typical
#    row.
# 3. Assume that the winning rectangle is squareish rather than long and thin.
#    This can be done by treating each row that doesn't have a single contiguous
#    red/green span that's at least say 20% of the overall width as a hard
#    barrier and only considering corner pairs that are both on the same side of
#    all barriers. Even ignoring the top and bottom of the diamond and assuming
#    the worst-case scenario of a single barrier exactly in the middle, the 
#    number of candidate rectangles is greatly reduced.
# 4. Check the candidate regions in order from largest to smallest area. (A
#    no-brainer, which would also be part of the approaches that didn't work.)
#    The first (largest) region that wasn't rejected by the previous step and
#    only contains red and green cells is the solution.

Coord = Data.define(:x, :y)

# edge_cells should be a hash from coordinate to type, where type is one of:
# |: a green cell on a vertical edge
# -: a green cell on a horizontal edge
# F: a red cell with red neighbors in the +x and +y directions
# 7: a red cell with red neighbors in the -x and +y directions
# J: a red cell with red neighbors in the -x and -y directions
# L: a red cell with red neighbors in the +x and -y directions
Perimeter = Data.define(:xrange, :yrange, :edge_cells)


# Returns an array of coords for red tiles in the input
def parse(input)
	input.split("\n").map { |line| 
		x, y = line.split(",").map(&:to_i)
		Coord.new(x, y)
	}
end


def solve(reds)
	perimeter = perimeter_from_reds(reds)
	spans_by_row = find_included_spans(perimeter)
	
	partition_y_ranges = partition_by_min_width(spans_by_row,
		perimeter.yrange.size * 0.2)
	partitions = partition_y_ranges.map { |yrange|
		reds.filter { |c| yrange.include?(c.y) }
	}
	
	candidate_corner_pairs = partitions
		.flat_map { |corners| corners.combination(2).to_a }
		.sort_by { |a, b| -1 * area(a, b) }
			
	best = candidate_corner_pairs
		.find { |a, b| valid_rect?(a, b, spans_by_row) }
	
	if best.nil?
		raise "No solution"
	end
	
	{ corners: best, area: area(*best) }
end


def perimeter_from_reds(reds)
	# This looks flagrantly inefficient, but there's no measurable cost vs a
	# single loop.
	min_x = reds.min_by { |coord| coord.x }.x
	max_x = reds.max_by { |coord| coord.x }.x
	min_y = reds.min_by { |coord| coord.y }.y
	max_y = reds.max_by { |coord| coord.y }.y
	
	edge_cells = Hash.new
	
	reds.length.times do |i|
		tile = reds[i]
		pred = reds[(i + 1) % reds.length]
		succ = reds[i - 1] # automatically wraps around
		
		if (pred.x == tile.x && succ.x == tile.x) ||
				(pred.y == tile.y && succ.y == tile.y)
			raise "Invalid input shape"
		end
		
		edge_cells[tile] = corner_type(tile, pred, succ)
		
		if tile.x == succ.x
			# Add vertical line
			s = tile.y
			e = succ.y
			s, e = [e, s] if s > e
			(s+1..e-1).each do |y|
				edge_cells[Coord.new(tile.x, y)] = "|"
			end
		else
			# Add horizontal line
			s = tile.x
			e = succ.x
			s, e = [e, s] if s > e
			(s+1..e-1).each do |x|
				edge_cells[Coord.new(x, tile.y)] = "-"
			end
		end
	end

	Perimeter.new(min_x..max_x, min_y..max_y, edge_cells)
end

def corner_type(tile, pred, succ)
	if pred.x < tile.x || succ.x < tile.x
		if pred.y < tile.y || succ.y < tile.y
			"J"
		else
			"7"
		end
	else
		if pred.y < tile.y || succ.y < tile.y
			"L"
		else
			"F"
		end
	end
end


def partition_by_min_width(spans_by_row, min_width)
	partitions = []
	current = nil
	
	spans_by_row.sort.each do |y, spans|
		# Is this row a barrier?
		if spans.none? { |s| s.size >= min_width }
			unless current.nil?
				current[1] = y - 1
				current = nil
			end
		else
			if current.nil?
				current = [y, y]
				partitions.push(current)
			else
				current[1] = y
			end
		end
	end
	
	partitions.map { |s, e| s..e}
end


def valid_rect?(corner_a, corner_b, spans_by_row)
	min_y, max_y = [corner_a.y, corner_b.y].sort
	
	# A rectangle is valid if all of its cells, including border, are red or
	# green. If there are any corners inside the rectangle's border, then the
	# border or interior must contain cells that are neither red nor green.
	(min_y..max_y).all? { |y|
		spans_by_row[y].any? { |span| 
			span.include?(corner_a.x) && span.include?(corner_b.x)
		}
	}
end


def area(corner_a, corner_b)
	w = (corner_a.x - corner_b.x).abs + 1
	h = (corner_a.y - corner_b.y).abs + 1
	w * h
end

# Returns a hash from y value to an array of spans
# Puzzle input has a large number of empty rows at the start. Using an array
# would mean either having to fill in a bunch of dummy values, which costs
# noticeable time, or offset the y coordinates.
def find_included_spans(perimeter)
	edge_cells_by_y = perimeter.edge_cells.group_by { |k, _| k.y}
	result = Hash.new
	
	perimeter.yrange.each do |y|
		# "Inside" and "there's a current span" are separate concepts:
		# * While inside is true, any cell that's not part of the perimeter
		#   is green and thus is included in a span.
		# * Any red/green perimeter cell remains red/green regardless of the
		#   value of inside and thus is included in a span.
		# * There's a current span (i.e. current_span is non-nil) if the
		#   previous cell was either inside or part of the perimeter.
		inside = false
		# current_span will be either nil or a [start, end] array. When a span
		# is finalized, it'll be converted to a Range object and pushed into
		# spans. That avoids the cost of instantiating a new Range object every
		# time a span is extended.
		current_span = nil
		spans = []
		
		# Examples:
		# x=012345678
		#    F--J   | ends up being a single span from F to -
		#    | F--7 | ends up being a single span from | to |
		#    L-J  L-J ends up being two spans, from L to J and from L to J
		#
		# In the first example:
		# 1. At x=1 (F), create a span.
		# 2. At x=2-3 (-), extend the current span.
		# 3. At x=4 (J), extend the current span and toggle inside to true.
		# 4. At x=8 (|), extend the current span and toggle inside to false.
		#
		# In the second example:
		# 1. At x=1 (|), create a span and toggle inside to true.
		# 2. At x=3 (F), extend the current span.
		# 3. At x=4-5 (-), extend the current span.
		# 4. At x=6 (7), extend the current span.
		# 5. At x=8 (|), extend the current span and toggle inside to false.
		#
		# In the third example:
		# 1. At x=1 (L), create a span and toggle inside to true.
		# 2. At x=2 (-), extend the current span.
		# 3. At x=3 (J), extend the current span and toggle inside to false.
		# 4. At x=6 (L), create a new span (because inside is false and
		#    the current cell is non-contiguious with the current span) and
		#    toggle inside to true.
		# 5. At x=7 (-), extend the current span.
		# 6. At x=8 (J), extend the current span and toggle inside to false.

		edge_cells_by_y[y].sort_by { |k, _| k.x }.each do |k, v|
			x = k.x

			if current_span.nil?
				current_span = [x, x]
			elsif inside || current_span[1] == x - 1
				current_span[1] = x
			else
				spans.push(current_span[0]..current_span[1])
				current_span = [x, x]
			end
			
			if v == "|" || v == "L" || v == "J"
				inside = !inside
			end
		end
		
		unless current_span.nil?
			spans.push(current_span[0]..current_span[1])
		end
				
		result[y] = spans
	end
	
	result
end


class Tests < Microtest::Test
	def test_parse
		expected = [
			Coord.new(7, 1),
			Coord.new(11, 1),
			Coord.new(11, 7),
			Coord.new(9, 7),
			Coord.new(9, 5),
			Coord.new(2, 5),
			Coord.new(2, 3),
			Coord.new(7, 3)
		]
		actual = parse(sample_input)
		assert_equal(expected, actual)
	end
	
	def test_perimeter_from_reds
		reds = [
			Coord.new(7, 1),
			Coord.new(11, 1),
			Coord.new(11, 7),
			Coord.new(9, 7),
			Coord.new(9, 5),
			Coord.new(2, 5),
			Coord.new(2, 3),
			Coord.new(7, 3)
		]
		expected = sample_perimeter
		actual = perimeter_from_reds(parse(sample_input))
		assert_equal(expected.edge_cells, actual.edge_cells)
		assert_equal(expected.xrange, actual.xrange, "xrange")
		assert_equal(expected.yrange, actual.yrange, "yrange")
	end
	
	def test_find_included_spans_inside_corners
		expected = {
			# skip y=0 because it's out of range
			1 => [7..11],
			2 => [7..11],
			3 => [2..11], # both sides of corner at x=7 are inside
			4 => [2..11],
			5 => [2..11], # both sides of corner at x=9 are inside
			6 => [9..11],
			7 => [9..11]
		}
		assert_equal(expected, find_included_spans(sample_perimeter))
	end
	
	def test_find_included_spans_outside_corners
		# F-------7
		# |XXF--7X|
		# |XX|..|X|
		# L--J..L.J
		perimeter = Perimeter.new(1..9, 0..3, {
			Coord.new(1, 0) => "F",
			Coord.new(2, 0) => "-",
			Coord.new(3, 0) => "-",
			Coord.new(4, 0) => "-",
			Coord.new(5, 0) => "-",
			Coord.new(6, 0) => "-",
			Coord.new(7, 0) => "-",
			Coord.new(8, 0) => "-",
			Coord.new(9, 0) => "7",
			
			Coord.new(1, 1) => "|",
			Coord.new(4, 1) => "F",
			Coord.new(5, 1) => "-",
			Coord.new(6, 1) => "-",
			Coord.new(7, 1) => "7",
			Coord.new(9, 1) => "|",
			
			Coord.new(1, 2) => "|",
			Coord.new(4, 2) => "|",
			Coord.new(7, 2) => "|",
			Coord.new(9, 2) => "|",
			
			Coord.new(1, 3) => "L",
			Coord.new(2, 3) => "-",
			Coord.new(3, 3) => "-",
			Coord.new(4, 3) => "J",
			Coord.new(7, 3) => "L",
			Coord.new(8, 3) => "-",
			Coord.new(9, 3) => "J",
		})
		expected = {
			0 => [1..9],
			1 => [1..9],
			2 => [1..4, 7..9],
			3 => [1..4, 7..9]
		}
		assert_equal(expected, find_included_spans(perimeter))
	end
	
	def test_partition_by_min_width
		# 012345678901234567890123456789012345678901234567890123456789
		# |----------|  (12, just wide enough)
		# |----------|  (12, just wide enough)
		# |---------|   (widest span is 11, not quite enough)     |--|
		# |-|           (not even close)
		# |----------|  (12, just wide enough)
		spans = {
			0 => [1..12],
			1 => [1..12],
			2 => [1..11, 57..60],
			3 => [1..3],
			4 => [1..12]
		}
		expected = [0..1, 4..4]
		assert_equal(expected, partition_by_min_width(spans, 12))
	end
	
	def sample_input
		File.read("../inputs/9-sample")
	end
	
	def sample_perimeter
		edge_cells = {
			Coord.new(7, 1) => "F",
			Coord.new(8, 1) => "-",
			Coord.new(9, 1) => "-",
			Coord.new(10, 1) => "-",
			Coord.new(11, 1) => "7",
			
			Coord.new(7, 2) => "|",
			Coord.new(11, 2) => "|",
			
			Coord.new(2, 3) => "F",
			Coord.new(3, 3) => "-",
			Coord.new(4, 3) => "-",
			Coord.new(5, 3) => "-",
			Coord.new(6, 3) => "-",
			Coord.new(7, 3) => "J",
			Coord.new(11, 3) => "|",
			
			Coord.new(2, 4) => "|",
			Coord.new(11, 4) => "|",
			
			Coord.new(2, 5) => "L",
			Coord.new(3, 5) => "-",
			Coord.new(4, 5) => "-",
			Coord.new(5, 5) => "-",
			Coord.new(6, 5) => "-",
			Coord.new(7, 5) => "-",
			Coord.new(8, 5) => "-",
			Coord.new(9, 5) => "7",
			Coord.new(11, 5) => "|",
			
			Coord.new(9, 6) => "|",
			Coord.new(11, 6) => "|",
			
			Coord.new(9, 7) => "L",
			Coord.new(10, 7) => "-",
			Coord.new(11, 7) => "J"
		}
		Perimeter.new(2..11, 1..7, edge_cells)
	end
end

Microtest.run(Tests.new)
reds = parse(ARGF.read)
pp(solve(reds))