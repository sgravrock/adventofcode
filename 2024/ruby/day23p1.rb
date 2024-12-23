#!/usr/bin/env ruby

# Turns an array of edges into a hash from nodes to neighbors
def build_graph(edges)
	graph = Hash.new

	edges.each do |a, b|
		[a, b].each do |n|
			if !graph.include?(n)
				graph[n] = []
			end
		end

		graph[a].push(b)
		graph[b].push(a)
	end

	graph
end

def cliques(graph)
	result = Set.new

	graph.each do |n1, neighbors|
		neighbors.combination(2).each do |n2, n3|
			if graph[n2].include?(n3)
				result.add(Set.new([n1, n2, n3]))
			end
		end
	end

	result
end

def solve(graph)
	cliques(graph)
		.filter { |clique|
			clique.any? { |node| node.start_with?("t") }
		}
		.count
end

edges = ARGF.read.split("\n")
	.map { |line| line.split("-") }
puts(solve(build_graph(edges)))
