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

	def next_free_block(start)
		pos = start - 1

		while pos < nblocks
			if allocation_map[pos].nil?
				return pos
			end

			pos += 1
		end

		raise "Could not find next free block after #{start}"
	end


	def prev_allocated_block(start)
		pos = start - 1

		while pos >= 0
			if !allocation_map[pos].nil?
				return pos
			end

			pos -= 1
		end
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

DefragState = Struct.new(:first_free_block, :last_allocated_block) do
	def done?
		first_free_block > last_allocated_block
	end

	class << self
		def from_fs(fs)
			DefragState.new(
				fs.next_free_block(-1),
				fs.prev_allocated_block(fs.nblocks)
			)
		end
	end
end


def defrag(fs)
	state = DefragState.from_fs(fs)

	until state.done?
		defrag_one_block(fs, state)
	end
end

def defrag_one_block(fs, state)
	to_move = state.last_allocated_block
	dest = state.first_free_block
	file_id = fs.allocation_map[to_move]
	ix_in_file = fs.files[file_id].blocks.index(to_move)

	if ix_in_file.nil?
		raise "inconsistency: allocation map and file disagree"
	end

	fs.allocation_map[dest] = file_id
	fs.allocation_map[to_move] = nil
	fs.files[file_id].blocks[ix_in_file] = dest

	state.first_free_block = fs.next_free_block(state.first_free_block)
	state.last_allocated_block = fs.prev_allocated_block(state.last_allocated_block)
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

	def test_defrag_one_block
		fs = Filesystem::from_disk_map("12345")
		state = DefragState.from_fs(fs)
		defrag_one_block(fs, state)
		assert_equal(false, state.done?)
		assert_equal(2, state.first_free_block, "first free block")
		assert_equal(13, state.last_allocated_block, "last allocated block")
		expected_allocation_map = [
			0,
			2,
			nil,
			1, 1, 1,
			nil, nil, nil, nil,
			2, 2, 2, 2,
			nil
		]
		assert_equal(expected_allocation_map, fs.allocation_map, "allocation_map")
		assert_equal([10, 11, 12, 13, 1], fs.files[2].blocks, "blocks of file 2")
	end

	def test_defrag
		fs = Filesystem::from_disk_map("12345")
		defrag(fs)
		expected_allocation_map = [
			0,
			2, 2,
			1, 1, 1,
			2, 2, 2,
			nil, nil, nil, nil, nil, nil
		]
		assert_equal(expected_allocation_map, fs.allocation_map)
	end
end


Microtest.run(Tests.new)

fs = Filesystem.from_disk_map(ARGF.read.chomp)
defrag(fs)
puts(fs.checksum)
