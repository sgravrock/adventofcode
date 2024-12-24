#!/usr/bin/env ruby

class Input
	attr_reader :name, :value

	def initialize(name, value)
		@name = name
		@value = value
	end

	def evaluate(ignored)
		@value
	end
end

class Gate
	attr_reader :name, :operation, :operand1, :operand2

	def initialize(name, operation, operand1, operand2)
		@name = name
		@operation = operation
		@operand1 = operand1
		@operand2 = operand2
		@value = nil
	end

	def evaluate(lines)
		if @value.nil?
			v1 = lines[@operand1].evaluate(lines)
			v2 = lines[@operand2].evaluate(lines)

			@value = case @operation
			when "AND"
				v1 & v2
			when "OR"
				v1 | v2
			when "XOR"
				v1 ^ v2
			else
				raise "Invalid operation #{@operation}"
			end
		end

		@value
	end

	def to_s
		"<gate name=#{name}: #{@operand1} #{@operation} #{@operand2}>"
	end
end

def to_dec(bits)
	bits.reduce(0) { |memo, bit| (memo << 1) | bit }
end

lines = Hash.new
chunks = ARGF.read.split("\n\n")
chunks[0]
	.split("\n")
	.each do |line|
		fields = line.split(": ")
		input = Input.new(fields[0], fields[1].to_i)
		lines[input.name] = input
	end

chunks[1]
	.split("\n")
	.each do  |line|
		fields = line.split(" ")
		gate = Gate.new(fields[4], fields[1], fields[0], fields[2])
		lines[gate.name] = gate
	end

zvals = lines
	.filter { |name, _| name.start_with?("z") }
	.sort_by { |name, _| name }
	.reverse
	.map { |_, line| line.evaluate(lines) }
puts(to_dec(zvals))
