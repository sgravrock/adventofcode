#!/usr/bin/env ruby

Puzzle = Data.define(:shape_areas, :regions)
Region = Data.define(:area, :requirements)

def parse(input)
	shape_areas = []
	regions = []
	
	input.split("\n").each do |line|
		if (line =~ /^[0-9]:/) != nil
			shape_areas.push(0)
		elsif (line =~ /^([0-9]+)x([0-9]+): (.+)/) != nil
			regions.push(Region.new($1.to_i * $2.to_i, $3.split(" ").map(&:to_i)))
		elsif line != ""
			shape_areas[shape_areas.length - 1] += line.count("#")
		end
	end
	
	return Puzzle.new(shape_areas, regions)
end

def trivially_infeasible(region, shape_areas)
	required_area = region.requirements
		.each_with_index
		.map { |n, i| n * shape_areas[i] }
		.sum
	required_area > region.area
end

puzzle = parse(ARGF.read)

# This give the wrong answer for the sample input, but the right answer for my
# actual input. IHBT.
maybes = puzzle.regions.reject { |r| 
	trivially_infeasible(r, puzzle.shape_areas)
}
puts(maybes.length)
