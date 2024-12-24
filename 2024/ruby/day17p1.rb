#!/usr/bin/env ruby

require './microtest'

# Opcodes
ADV = 0
BXL = 1
BST = 2
JNZ = 3
BXC = 4
OUT = 5
BDV = 6
CDV = 7

Machine = Struct.new(:registers, :program, :ip, :output) do
	def execute(max_ticks)
		ticks = 0

		while ip >= 0 && ip < program.length do
			if ticks > max_ticks then
				raise "Tick limit exceeded"
			end

			step
			ticks += 1
		end

		puts "Finished in #{ticks} ticks"
	end

	def step
		opcode = program[ip]
		operand = program[ip + 1]
		jumped = false

		case opcode
		when ADV
			operand = evaluate_combo_operand(operand)
			registers[:A] = registers[:A] / 2.pow(operand)
		when BDV
			operand = evaluate_combo_operand(operand)
			registers[:B] = registers[:A] / 2.pow(operand)
		when CDV
			operand = evaluate_combo_operand(operand)
			registers[:C] = registers[:A] / 2.pow(operand)
		when BXL
			registers[:B] = registers[:B] ^ operand
		when BXC
			registers[:B] = registers[:B] ^ registers[:C]
		when BST
			operand = evaluate_combo_operand(operand)
			registers[:B] = operand % 8
		when JNZ
			if registers[:A] != 0
				self.ip = operand
				jumped = true
			end
		when OUT
			raw = operand
			operand = evaluate_combo_operand(operand)
			#puts "OUT Pushing #{operand} % 8 = #{operand % 8} (was #{raw})"
			output.push(operand % 8)
		else
			raise "Unimplemented opcode #{opcode}"
		end
		
		self.ip += 2 unless jumped
	end

	def evaluate_combo_operand(operand)
		if operand >= 0 && operand <= 3 then
			operand
		elsif operand == 4 then
			registers[:A]
		elsif operand == 5 then
			registers[:B]
		elsif operand == 6 then
			registers[:C]
		else
			raise "Invalid combo operand #{operand}"
		end
	end


	class << self
		def parse(input)
			chunks = input.split("\n\n")

			register_re = /^Register (.): ([0-9]+)$/
			registers = chunks[0]
				.split("\n")
				.map { |line|
					m = register_re.match(line)
					[m[1].to_sym, m[2].to_i]
				}
				.to_h

			program = chunks[1]
				.sub("Program: ", "")
				.split(",")
				.map(&:to_i)

			Machine.new(registers, program, 0, [])
		end
	end
end

class Tests < Microtest::Test
	def test_combo_literals
		m = make_machine({A: 10, B: 10, C: 10}, [])

		[0, 1, 3, 3].each do |i|
			assert_equal(i, m.evaluate_combo_operand(i), "operand value #{i}")
		end
	end

	def test_combo_a
		m = make_machine({A: 42}, [])
		assert_equal(42, m.evaluate_combo_operand(4))
	end

	def test_combo_b
		m = make_machine({B: 42}, [])
		assert_equal(42, m.evaluate_combo_operand(5))
	end

	def test_combo_c
		m = make_machine({C: 42}, [])
		assert_equal(42, m.evaluate_combo_operand(6))
	end

	def test_adv
		m = make_machine({A: 20, B: 3}, [0, 5])
		m.step
		assert_equal(2, m.registers[:A])
	end

	def test_bdv
		m = make_machine({A: 20, B: 3}, [6, 5])
		m.step
		assert_equal(2, m.registers[:B])
	end

	def test_cdv
		m = make_machine({A: 20, B: 3}, [7, 5])
		m.step
		assert_equal(2, m.registers[:C])
	end

	def test_bxl
		m = make_machine({B: 5}, [1, 3])
		m.step
		assert_equal(6, m.registers[:B])
	end

	def test_bst
		m = make_machine({C: 12}, [2, 6])
		m.step
		assert_equal(4, m.registers[:B])
	end

	def test_jnz_no_jump
		m = make_machine({A: 0}, [3, 100])
		m.step
		assert_equal(2, m.ip)
	end

	def test_jnz_jump
		m = make_machine({A: 1}, [3, 100])
		m.step
		assert_equal(100, m.ip)
	end

	def test_bxc
		m = make_machine({B: 5, C: 3}, [4, 0])
		m.step
		assert_equal(6, m.registers[:B])
	end

	def test_out
		m = make_machine({A: 12}, [5, 4, 5, 3])
		m.step
		assert_equal([4], m.output)
		m.step
		assert_equal([4, 3], m.output)
	end

	def test_example_1
		m = make_machine({C: 9}, [2, 6])
		m.execute(2)
		assert_equal(1, m.registers[:B])
	end

	def test_example_2
		m = make_machine({A: 10}, [5,0,5,1,5,4])
		m.execute(3)
		assert_equal([0,1,2], m.output)
	end

	def make_machine(registers, program)
		[:A, :B, :C].each do |r|
			unless registers.include?(r)
				registers[r] = 0
			end
		end

		Machine.new(registers, program, 0, [])
	end
end

Microtest.run(Tests.new)

max_ticks = 100000
m = Machine.parse(ARGF.read)
puts m
m.execute(max_ticks)
puts(m.output.join(","))
