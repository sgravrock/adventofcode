#!/usr/bin/env ruby

def next_num(num)
	ops = [
		-> (n) {n * 64},
		-> (n) { n / 32},
		-> (n) {n * 2048}
	]

	ops.reduce(num) { |memo, op|
		(memo ^ op.call(memo)) % 16777216
	}
end

def nth(initial_num, iterations)
	iterations.times
		.reduce(initial_num) { |memo, _| next_num(memo) }
end

result = STDIN.read.split("\n")
	.map(&:to_i)
	.map { |n| nth(n, 2000) }
	.sum

puts(result)
