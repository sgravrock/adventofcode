#!/usr/bin/env ruby

require './microtest'

XY = Struct.new(:x, :y)

Robot = Struct.new(:position, :velocity) do
	def move(bounds)
		newpos = XY.new(
			(self.position.x + self.velocity.x) % bounds.x,
			(self.position.y + self.velocity.y) % bounds.y,
		)

		if newpos.x < 0 || newpos.x >= bounds.x || newpos.y < 0 || newpos.y >= bounds.y
			raise "Tried to move #{self} out-of-bounds position #{newpos}"
		end

		self.position = newpos
	end
end

def safety_factor(robots, bounds)
	if bounds.x % 2 == 0 
		raise "Width must not be even"
	end

	if bounds.y % 2 == 0 
		raise "Height must not be even"
	end

	quadrant_counts = [[0, 0], [0, 0]]

	robots.each do |r|
		pos = r.position

		if pos.y < bounds.y / 2
			row = quadrant_counts[0]
		elsif pos.y > bounds.y / 2
			row = quadrant_counts[1]
		else
			next
		end

		if pos.x < bounds.x / 2
			row[0] += 1
		elsif pos.x > bounds.x / 2
			row[1] += 1
			next
		end
	end

	quadrant_counts[0][0] * quadrant_counts[0][1] *
		quadrant_counts[1][0] * quadrant_counts[1][1]
end

def print_grid(robots, bounds)
	bounds.y.times do |y|
		bounds.x.times do |x|
			c = XY.new(x, y)
			n = robots.count { |r| r.position == c }
			
			if n == 0
				print "."
			else
				print n
			end
		end

		puts
	end
end

class Tests < Microtest::Test
	def test_safety_factor
		robot_coords = [
			XY.new(6, 0),
			XY.new(6, 0),
			XY.new(9, 0),
			XY.new(0, 2),
			XY.new(3, 5),
			XY.new(4, 5),
			XY.new(4, 5),
			XY.new(1, 6),
			XY.new(6, 6),
		]
		robots = robot_coords.map { |c| Robot.new(c, nil) }
		assert_equal(12, safety_factor(robots, XY.new(11, 7)))
	end

	def test_move_no_teleport
		r = Robot.new(XY.new(2, 4), XY.new(2, -3))
		r.move(XY.new(11, 7))
		assert_equal(XY.new(4, 1), r.position)
	end

	def test_move_teleport_x
		r = Robot.new(XY.new(10, 6), XY.new(2, -3))
		r.move(XY.new(11, 7))
		assert_equal(XY.new(1, 3), r.position)
	end

	def test_move_teleport_y
		r = Robot.new(XY.new(4, 1), XY.new(2, -3))
		r.move(XY.new(11, 7))
		assert_equal(XY.new(6, 5), r.position)
	end
end

Microtest.run(Tests.new)

if ARGV.length != 3
	puts "Usage: #{$0} input-file width height"
	exit(1)
end

robots = File.read(ARGV[0])
	.split("\n")
	.map { |line|
		re = /^p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)$/
		m = re.match(line)
		Robot.new(XY.new(m[1].to_i, m[2].to_i), XY.new(m[3].to_i, m[4].to_i))
	}
bounds = XY.new(ARGV[1].to_i, ARGV[2].to_i)

100.times do |_|
	robots.each do |r|
		r.move(bounds)
	end
end

# print_grid(robots, bounds)
puts(safety_factor(robots, bounds))
