#!/usr/bin/env ruby

require './microtest'

# Allocation map is an array of integers or nil
# Integers are indices into files
# nil means the block is free
Filesystem = Struct.new(:nblocks, :allocation_map, :files) do
	def checksum
		allocation_map
			.each_with_index
			.filter { |file_id, _| !file_id.nil? }
			.map { |file_id, block_ix| file_id * block_ix }
			.reduce(:+)
	end

	class << self
		def from_disk_map(disk_map)
			map_nums = disk_map.chars.map(&:to_i)
			nblocks = map_nums.sum
			allocation_map = Array.new(nblocks, nil)
			files = []
			next_block = 0

			map_nums.each_with_index do |n, i|
				if i % 2 == 0
					# A file with n blocks
					file_id = i / 2
					blocks = []

					n.times do ||
						blocks.push(next_block)
						allocation_map[next_block] = file_id
						next_block += 1
					end
					
					if files.length != file_id
						raise "inconsistency: expected #{file_id} files but have #{files.length}"
					end

					files.push(Day9File.new(file_id, blocks))
				else
					# n blocks of free space
					next_block += n
				end
			end

			return Filesystem.new(nblocks, allocation_map, files)
		end
	end
end

Day9File = Struct.new(:id, :blocks)

def defrag(fs)
	fs.files.each_index.reverse_each do |file_id|
		defrag_file(fs, file_id)
	end
end

def defrag_file(fs, file_id)
	f = fs.files[file_id]
	new_start_block = first_free_run(fs, f.blocks.length)

	if new_start_block.nil? || new_start_block > f.blocks[0]
		return
	end

	puts "Moving file #{file_id} to block #{new_start_block}"

	f.blocks.each do |bi|
		fs.allocation_map[bi] = nil
	end

	f.blocks.length.times do |i|
		f.blocks[i] = new_start_block + i
		fs.allocation_map[new_start_block + i] = file_id
	end
end

def first_free_run(fs, length)
	# TODO optimize
	(0...(fs.nblocks - length))
		.filter { |start|
			length.times.all? { |i| fs.allocation_map[start + i].nil? }
		}
		.first
end


class Tests < Microtest::Test
	def test_filesystem_from_disk_map
		fs = Filesystem::from_disk_map("12345")
		assert_equal("0..111....22222".length, fs.nblocks, "nblocks")
		expected_allocation_map = [
			0,
			nil, nil,
			1, 1, 1,
			nil, nil, nil, nil,
			2, 2, 2, 2, 2
		]
		assert_equal(expected_allocation_map, fs.allocation_map, "allocation_map")
		assert_equal(3, fs.files.length, "number of files")
		assert_equal([0], fs.files[0].blocks, "blocks of file 0")
		assert_equal([3, 4, 5], fs.files[1].blocks, "blocks of file 1")
		assert_equal([10, 11, 12, 13, 14], fs.files[2].blocks, "blocks of file 2")
	end

	def test_defrag_file
		fs = Filesystem::from_disk_map("2333133121414131402")
		defrag_file(fs, 9)
		assert_equal(9, fs.allocation_map[2], "block 2 allocation")
		assert_equal(9, fs.allocation_map[3], "block 3 allocation")
		assert_equal([2, 3], fs.files[9].blocks, "blocks of file 9")
	end

	def test_defrag
		fs = Filesystem::from_disk_map("2333133121414131402")
		defrag(fs)
		expected_allocation_map = [
			0, 0,
			9, 9,
			2,
			1, 1, 1,
			7, 7, 7,
			nil,
			4, 4,
			nil,
			3, 3, 3,
			nil, nil, nil, nil,
			5, 5, 5, 5,
			nil, 
			6, 6, 6, 6,
			nil, nil, nil, nil, nil,
			8, 8, 8, 8,
			nil, nil
		]
		assert_equal(expected_allocation_map, fs.allocation_map)
	end
end


Microtest.run(Tests.new)

fs = Filesystem.from_disk_map(ARGF.read.chomp)
defrag(fs)
puts(fs.checksum)
