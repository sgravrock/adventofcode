#!/usr/bin/env ruby

def max_except_last(digits)
	best_i = 0

	(1...(digits.length - 1)).each do |i|
		if digits[i] > digits[best_i]
			best_i = i
		end
	end

	[digits[best_i], best_i]
end

p ARGF.read.split("\n")
	.map { |line|
		digits = line.split('').map(&:to_i)
		a, i = max_except_last(digits)
		b = digits[(i + 1)..].max
		a*10 + b
	}
	.sum
