#!/usr/bin/env ruby

blocks = ARGF.read.split("\n\n")
ranges = blocks[0]
	.split("\n")
	.map { |line|
		s, e = line.split("-")
		(s.to_i..e.to_i)
	}
	.sort_by(&:begin)

result = 0
i = 0

ranges.each do |range|
	next if i > range.end
	i = [i, range.begin].max
	result += range.end - i + 1
	i = range.end + 1	
end

puts(result)