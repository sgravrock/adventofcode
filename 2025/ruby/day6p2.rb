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
	cols = input.split("\n").map(&:chars).transpose
	problems = []
	
	cols.each do |cells|
		maybe_operator = cells.last
		if maybe_operator == '*' || maybe_operator == '+' then
			problems.push(Problem.new(maybe_operator, []))
			cells.pop
		end
		
		cells = cells.reject { |c| c == ' '}
		unless cells.empty?
			n = cells.join("").to_i
			problems.last.operands.push(cells.join("").to_i)
		end
	end
	
	problems
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
			Problem.new('*', [1, 24, 356]),
			Problem.new('+', [369, 248, 8]),
			Problem.new('*', [32, 581, 175]),
			Problem.new('+', [623, 431, 4])
		]
		assert_equal(expected, parse(input))
	end
end

Microtest.run(Tests.new)
problems = parse(ARGF.read)
puts(problems.map(&:result).sum)