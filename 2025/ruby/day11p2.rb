#!/usr/bin/env ruby

def parse(input)
	h = Hash.new
	input.split("\n").each do |line|
		from, tos = line.split(": ")
		h[from] = tos.split(" ")
	end
	h
end

class Pathfinder
	def initialize(graph, dest)
		@graph = graph
		@dest = dest
		@memo = Hash.new
	end
	
	def find(start, requirements)
		key = [start, requirements]
		if @memo[key].nil?
			@memo[key] = find_raw(start, requirements)
		end
		@memo[key]
	end
	
	private
	
	def find_raw(start, requirements)
		if start == @dest
			if requirements.empty?
				1
			else
				0
			end
		else
			nr = requirements.reject { |x| x == start }
			@graph[start]
				.map { |nn| find(nn, nr) }
				.sum
		end
	end
end

def solve(graph)
	Pathfinder.new(graph, "out").find("svr", ["dac", "fft"])
end


puts(solve(parse(ARGF.read)))