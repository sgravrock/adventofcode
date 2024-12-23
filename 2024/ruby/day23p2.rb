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

def solve(graph)
	max_cliques(graph)
		.max_by { |c| c.size }
		.sort
		.join(",")
end

def max_cliques(graph)
	result = []
	bron_kerbosch(Set.new, Set.new(graph.keys), Set.new, graph, result)
	result
end

def bron_kerbosch(r, p, x, graph, out)
	if p.empty? && x.empty?
		out.push(r.clone)
	else
		p.each do |v|
			ns = graph[v]
			bron_kerbosch(r.union([v]), p.intersection(ns), x.intersection(ns),
				graph, out)
			p.delete(v)
			x.add(v)
		end
	end
end

edges = ARGF.read.split("\n")
	.map { |line| line.split("-") }
graph = build_graph(edges)
puts(solve(graph))
