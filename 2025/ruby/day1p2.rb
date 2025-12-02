#!/usr/bin/env ruby

pos = 50
passwd = 0

ARGF.read.split("\n").each do |ins|
	dir = ins[0]
	count = ins[1..].to_i
	delta = if dir == 'L' then
		-1
	else
		1
	end

	(1..count).each do ||
		pos = (pos + delta) % 100
	
		if pos == 0 then
			passwd += 1
		end
	end
end

p passwd
