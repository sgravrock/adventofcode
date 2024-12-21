#!/usr/bin/env ruby

def solve(patterns, designs)
	possibilities = Hash.new { |h, k|
		design = k

		h[k] = patterns
			.filter { |pat| design.start_with?(pat) }
			.map { |pat|
				suffix = design[(pat.length)..]

				if suffix == ''
					1
				else
					possibilities[suffix]
				end
			}
			.sum
	}

	designs	
		.map { |d| possibilities[d] }
		.sum
end


lines = STDIN.read.split("\n")
patterns = lines.shift.split(", ")
lines.shift # ignore blank
designs = lines
puts(solve(patterns, designs))

