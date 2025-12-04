#!/usr/bin/env ruby

require './microtest'

def best_joltage(s)
	digits = s.split('').map(&:to_i)
	result = 0
	pos = 0
	i = 12

	while i > 0 do
		dpos = select_next(digits, pos, i)
		i -= 1
		pos = dpos + 1
		result = result*10 + digits[dpos]
	end

	result
end


def select_next(digits, start, num_remaining)
	best_i = start

	((start + 1)..(digits.length - num_remaining)).each do |i|
		if digits[i] > digits[best_i]
			best_i = i
		end
	end

	best_i
end

class Tests < Microtest::Test
	def test_best_joltage
		cases = [
			['987654321111111', 987654321111],
			['811111111111119', 811111111119],
			['234234234234278', 434234234278],
			['818181911112111', 888911112111]
		]

		cases.each do |input, expected|
			assert_equal(expected, best_joltage(input), context=input)
		end
	end
end


Microtest.run(Tests.new)
p ARGF.read.split("\n")
	.map { |line| best_joltage(line) }
	.sum


