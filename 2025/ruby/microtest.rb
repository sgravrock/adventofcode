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

		any_excluded = tests.methods
			.any? {|symbol| symbol.to_s.start_with? 'xtest_' }

		if any_excluded then
			raise "Exiting because excluded tests (xtest_) were found"
			exit 2
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
		def assert_equal(expected, actual, context=nil)
			if expected != actual
				if state.ok then
					state.ok = false
					print "#{state.name} failed:"
					print " #{context}: " unless context.nil?
					puts
				elsif !context.nil? then
					puts "#{context}:"
				end

				if actual.is_a?(Set) && expected.is_a?(Set) then
					print_set_mismatch(expected, actual)
				elsif actual.is_a?(Hash) && expected.is_a?(Hash) then
					print_hash_mismatch(expected, actual)
				elsif contains_newline(actual) || contains_newline(expected)
					puts "   Expected\n#{actual.inspect}\n   to equal\n#{expected.inspect}"
				else
					puts "   Expected #{actual.inspect} to equal #{expected.inspect}"
				end
			end
		end

		private

		def print_hash_mismatch(expected, actual)
			missing = expected.filter { |k, _| !actual.include?(k) }
			extra = actual.filter { |k, _| !expected.include?(k) }
			mismatches = expected.keys.filter { |k|
				actual.include?(k) && actual[k] != expected[k]
			}

			unless missing.empty? then
				puts "   Expected hash to include:"
				missing.each do |k, v|
					puts "      #{k.inspect} => #{v.inspect}"
				end
			end

			unless extra.empty? then
				puts "   Expected hash not to include:"
				extra.each do |k, v|
					puts "      #{k.inspect} => #{v.inspect}"
				end
			end

			mismatches.each do |k|
				puts "   For key #{k.inspect}, expected #{actual[k].inspect} to equal #{expected[k].inspect}"
			end
		end

		def print_set_mismatch(expected, actual)
			missing = expected - actual
			extra = actual - expected

			unless missing.empty? then
				puts "   Expected set to include:"
				missing.each do |el|
					puts "      #{el.inspect}"
				end
			end

			unless extra.empty? then
				puts "   Expected set not to include:"
				extra.each do |el|
					puts "      #{el.inspect}"
				end
			end
		end

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
