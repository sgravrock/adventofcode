#!/usr/bin/env ruby

blocks = ARGF.read.split("\n\n")
ranges = blocks[0]
	.split("\n")
	.map { |line|
		s, e = line.split("-")
		(s.to_i..e.to_i)
	}
ingredients = blocks[1].split("\n")
	.map(&:to_i)

# Brute force is plenty fast enough
result = ingredients.count { |ing|
	ranges.any? { |rng| rng.include?(ing) }
}
puts(result)
