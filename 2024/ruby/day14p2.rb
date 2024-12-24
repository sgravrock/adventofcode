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

def has_vertical(robots, bounds)
	occupied = Array.new(bounds.y) { Array.new(bounds.x, false) }

	robots.each do |r|
		occupied[r.position.y][r.position.x] = true
	end

	bounds.y.times.any? { |y|
		consecutive = 0

		bounds.x.times.each do |x|
			if occupied[y][x]
				consecutive += 1
			else
				consecutive = 0
			end

			if consecutive == 10
				break x
			end
		end

		consecutive == 10
	}
end

def print_grid(robots, bounds)
	bounds.y.times do |y|
		bounds.x.times do |x|
			c = XY.new(x, y)
			n = robots.count { |r| r.position == c }
			
			if n == 0
				print " "
			else
				print n
			end
		end

		puts
	end
end

class Tests < Microtest::Test
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

if ARGV.length != 4
	puts "Usage: #{$0} input-file width height skips"
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
skips = ARGV[3].to_i
i = 1

while true do
	robots.each do |r|
		r.move(bounds)
	end

	if i > skips
		if has_vertical(robots, bounds)
			print_grid(robots, bounds)
			puts "Candidate: #{i}"
			exit(0)
		end
	end

	i += 1
end
