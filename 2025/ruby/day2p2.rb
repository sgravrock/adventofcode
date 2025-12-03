#!/usr/bin/env ruby

require './microtest'

def valid?(id)
	(1..id.length / 2).each do |chunklen|
		if id.length % chunklen == 0 then
			prefix = id[0...chunklen]
			n = id.length / chunklen
			if prefix * n == id then
				return false
			end
		end
	end

	true
end

def invalids(rangestr)
	chunks = rangestr.split('-')
	range = (chunks[0].to_i..chunks[1].to_i)
	range.filter { |i| !valid?(i.to_s) }
end

def solve(input)
	input.split(',')
		.flat_map { |r| invalids(r) }
		.sum
end


class Tests < Microtest::Test
	def test_valid?
		assert_equal(false, valid?('1188511885'))
		assert_equal(true, valid?('1188511884'))
		assert_equal(true, valid?('118851188'))
		assert_equal(false, valid?('222222'))

		assert_equal(false, valid?('12341234'))
		assert_equal(false, valid?('123123123'))
		assert_equal(false, valid?('1212121212'))
		assert_equal(false, valid?('1111111'))
	end

	def test_invalids
		assert_equal([11, 22], invalids('11-22'))
		assert_equal([99, 111], invalids('95-115'))
		assert_equal([222222], invalids('222220-222224'))
	end
end

Microtest.run(Tests.new)
puts solve(ARGF.read)
