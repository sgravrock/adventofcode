module Microtest
	def Microtest.run(tests)
		n_tests = 0
		n_failed = 0
		tests.methods
			.select {|symbol| symbol.to_s.start_with? 'test_' }
			.each do |symbol|
				tests.state = SingleTestState.new symbol.to_s
				tests.run_one_test symbol
				n_tests += 1

				unless tests.state.ok
					n_failed += 1
				end
			end

		puts
		if n_failed == 0
			puts "All #{n_tests} tests passed."
		else
			puts "#{n_failed} of #{n_tests} tests failed."
			exit 1
		end
	end

	class Test
		attr_accessor :state

		def run_one_test(symbol)
			begin
				public_send symbol
			rescue => e
				state.ok = false
				puts "#{state.name} failed:"
				puts e
				puts e.backtrace
			end
		end

		def assert_equal(expected, actual)
			if expected != actual
				if state.ok
					state.ok = false
					puts "#{state.name} failed:"
				end

				if contains_newline(actual) || contains_newline(expected)
					puts "   Expected\n#{actual}\n   to equal\n#{expected}"
				else
					puts "   Expected #{actual} to equal #{expected}"
				end
			end
		end

		private

		def contains_newline(value)
			value.respond_to?(:include?) && value.include?("\n")
		end
	end

	class SingleTestState
		attr_accessor :ok, :name

		def initialize(name)
			@ok = true
			@name = name
		end
	end
end
