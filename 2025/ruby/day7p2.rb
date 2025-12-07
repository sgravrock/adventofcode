#!/usr/bin/env ruby

rows = ARGF.read.split("\n").map(&:chars)
n_timelines = Hash.new(0)
n_timelines[[0, rows[0].length / 2]] = 1

(0...rows.length-1).each do |y|
	rows[y].each_with_index do |c, x|
		if c == 'S' || c == '|' then
			prev_n = n_timelines[[y, x]]
			
			if rows[y + 1][x] == '^' then
				rows[y + 1][x - 1] = '|'
				rows[y + 1][x + 1] = '|'
				n_timelines[[y +  1, x - 1]] += prev_n
				n_timelines[[y +  1, x + 1]] += prev_n
			else
				rows[y + 1][x]  = '|'
				n_timelines[[y +  1, x]] += prev_n
			end
		end
	end
	
	puts(rows.map { |r| r.join("") }.join("\n"))
	puts
end

result = (0...rows.last.length)
	.sum { |x| n_timelines[[rows.length - 1, x]] }

puts(result)