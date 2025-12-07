#!/usr/bin/env ruby

rows = ARGF.read.split("\n").map(&:chars)
n_splits = 0

(0...rows.length-1).each do |y|
	rows[y].each_with_index do |c, x|
		if c == 'S' || c == '|' then
			if rows[y + 1][x] == '^' then
				rows[y + 1][x - 1] = '|'
				rows[y + 1][x + 1] = '|'
				n_splits += 1
			else
				rows[y + 1][x]  = '|'
			end
		end
	end
	
	puts(rows.map { |r| r.join("") }.join("\n"))
	puts
end

puts(n_splits)