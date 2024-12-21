#!/usr/bin/env ruby

def solve(patterns, designs)
	designs.count { |d| possible?(d, patterns) }
end

def possible?(design, patterns)
	if design == ''
		return true
	end

	patterns.any? { |pat|
		design.start_with?(pat) &&
			possible?(design[(pat.length)..], patterns)
	}
end


lines = STDIN.read.split("\n")
patterns = lines.shift.split(", ")
lines.shift # ignore blank
designs = lines
puts(solve(patterns, designs))

