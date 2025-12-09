#!/usr/bin/env ruby

def build_distance_list(points)
	distances = []
	
	points.each_with_index do |p1, i|
		(i+1...points.length).each do |j|
			p2 = points[j]
			 dx = p1[0] - p2[0]
			 dy = p1[1] - p2[1]
			 dz = p1[2] - p2[2]
			 d = Math.sqrt(dx*dx + dy*dy + dz*dz)
			 distances.push([p1, p2, d])
		end
	end
	
	distances.sort_by { |p| p[2] }
end

points = ARGF.read.split("\n").map { |line|
	line.split(",").map(&:to_i)
}
distances = build_distance_list(points)
circuits = []
circuits_by_point = Hash.new

# Seed to simplify the subsequent code
points.each do |p|
	c = Set.new([p])
	circuits.push(c)
	circuits_by_point[p] = c
end

while circuits.size > 1
	p1, p2, d = distances.shift
	c1 = circuits_by_point[p1]
	c2 = circuits_by_point[p2]
	
	unless c1.equal?(c2) # same instances
		circuits.delete(c2)
		c2.each do |p|
			c1.add(p)
			circuits_by_point[p] = c1
		end
		
		last_two = [p1, p2]
	end
end

puts(last_two[0][0] * last_two[1][0])