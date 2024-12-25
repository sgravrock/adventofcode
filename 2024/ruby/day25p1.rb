#!/usr/bin/env ruby

def lock_heights(lines)
	heights = Array.new(lines[0].length, 0)

	(1...lines.length).each do |i|
		lines[i].chars.each_with_index do |c, j|
			if c == '#'
				heights[j] += 1
			end
		end
	end

	heights
end

def key_heights(lines)
	heights = Array.new(lines[0].length, 0)

	(0...lines.length - 1).reverse_each do |i|
		lines[i].chars.each_with_index do |c, j|
			if c == '#'
				heights[j] += 1
			end
		end
	end

	heights
end

def count_pairs(locks, keys)
	locks
		.map { |lock|
			keys.count { |key| fit?(lock, key) }
		}
		.sum
end

def fit?(lock, key)
	lock.zip(key).all? { |lp, kp| lp + kp <= 5 }
end

locks = []
keys = []
ARGF.read
	.split("\n\n")
	.each do |chunk|
		lines = chunk.split("\n")
		if lines[0][0] == '#'
			locks.push(lock_heights(lines))
		else
			keys.push(key_heights(lines))
		end
	end

puts(count_pairs(locks, keys))
