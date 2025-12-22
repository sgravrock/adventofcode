#!/usr/bin/env ruby

def parse(input)
	h = Hash.new
	input.split("\n").each do |line|
		from, tos = line.split(": ")
		h[from] = tos.split(" ")
	end
	h
end

def paths(graph, start, dest)
	if start == dest
		1
	else
		graph[start]
			.map { |nn| paths(graph, nn, dest) }
			.sum
	end
end

puts(paths(parse(ARGF.read), "you", "out"))