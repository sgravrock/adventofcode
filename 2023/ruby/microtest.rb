module Microtest
	def Microtest.run(tests)
		n_tests = 0
		n_failed = 0
		focused = true
		test_methods = tests.methods
			.select {|symbol| symbol.to_s.start_with? 'ftest_' }

		if test_methods.empty?
			focused = false
			test_methods = tests.methods
				.select {|symbol| symbol.to_s.start_with? 'test_' }
		end

		test_methods.each do |symbol|
			tests.state = SingleTestState.new symbol.to_s
			tests.run_one_test symbol
			n_tests += 1

			unless tests.state.ok
				n_failed += 1
			end
		end

		puts
		if focused then
			puts "#{n_failed} of #{n_tests} focused tests failed."
			exit 2
		elsif n_failed > 0
			puts "#{n_failed} of #{n_tests} tests failed."
			exit 1
		else
			puts "All #{n_tests} tests passed."
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

		def fail(msg)
			if state.ok
				state.ok = false
				puts "#{state.name} failed:"
			end

			puts "   #{msg}"
		end

		# TODO refactor, use fail()
		def assert_equal(expected, actual)
			if expected != actual
				if state.ok
					state.ok = false
					puts "#{state.name} failed:"
				end

				if contains_newline(actual) || contains_newline(expected)
					puts "   Expected\n#{actual.inspect}\n   to equal\n#{expected.inspect}"
				else
					puts "   Expected #{actual.inspect} to equal #{expected.inspect}"
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
