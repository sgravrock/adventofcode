#!/usr/bin/env ruby

pos = 50
passwd = 0

ARGF.read.split("\n").each do |ins|
	dir = ins[0]
	count = ins[1..].to_i
	if  dir == 'L' then
		count *= -1;
	end

	pos = (pos + count) % 100
	
	if pos == 0 then
		passwd += 1
	end
end

p passwd
