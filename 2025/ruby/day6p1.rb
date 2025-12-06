#!/usr/bin/env ruby
require './microtest'

Problem = Data.define(:operator, :operands) do
	def result
		case operator
		when '+'
			operands.reduce(&:+)
		when '*'
			operands.reduce(&:*)
		else
			raise "Unknown operator #{operator}"
		end
	end
	
	def inspect
		"(#{operator} #{operands})"
	end
end

def parse(input)
	input.split("\n").map(&:split)
		.transpose
		.map { |problem_spec| 
			operator = problem_spec.pop
			operands = problem_spec.map(&:to_i)
			Problem.new(operator, operands)
		}
end

class Tests < Microtest::Test
	def test_parse
		input = <<~END
			123 328  51 64 
			 45 64  387 23 
			  6 98  215 314
			*   +   *   +  
		END
		expected = [
			Problem.new('*', [123, 45, 6]),
			Problem.new('+', [328, 64, 98]),
			Problem.new('*', [51, 387, 215]),
			Problem.new('+', [64, 23, 314])
		]
		assert_equal(expected, parse(input))
	end
end

Microtest.run(Tests.new)
problems = parse(ARGF.read)
puts(problems.map(&:result).sum)