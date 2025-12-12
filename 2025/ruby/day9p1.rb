#!/usr/bin/env ruby

red_tiles = ARGF.read.split("\n").map { |line| line.split(",").map(&:to_i) }
result = 0

(0...red_tiles.length).each do |i|
	(i+1...red_tiles.length).each do |j|
		w = (red_tiles[i][0] - red_tiles[j][0]).abs + 1
		h = (red_tiles[i][1] - red_tiles[j][1]).abs + 1
		a = w * h
		result = a if a > result
	end
end

puts(result)