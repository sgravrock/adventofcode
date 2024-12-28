#!/usr/bin/env ruby

require './microtest'

# Allocation map is an array of integers or nil
# Integers are indices into files
# nil means the block is free
Filesystem = Struct.new(:nblocks, :free_list, :files) do
	def checksum
		files.sum { |f| f.checksum }
	end

	class << self
		def from_disk_map(disk_map)
			map_nums = disk_map.chars.map(&:to_i)
			nblocks = map_nums.sum
			free_chunks = []
			files = []
			next_block = 0

			map_nums.each_with_index do |n, i|
				if i % 2 == 0
					# A file with n blocks
					file_id = i / 2
					blocks = []

					n.times do ||
						blocks.push(next_block)
						next_block += 1
					end
					
					if files.length != file_id
						raise "inconsistency: expected #{file_id} files but have #{files.length}"
					end

					files.push(Day9File.new(file_id, blocks))
				else
					# n blocks of free space
					if n != 0
						free_chunks.push([next_block, n])
						next_block += n
					end
				end
			end

			return Filesystem.new(nblocks, FreeList.new(free_chunks), files)
		end
	end
end

class FreeList
	# Takes an array of [start, length] pairs
	def initialize(chunks)
		nodes = chunks.map { |start, len| FreeListNode.new(nil, nil, start, len) }
		nodes.each_with_index do |node, i|
			node.prev = nodes[i - 1] unless i == 0
			node.next = nodes[i + 1]
		end

		@head = nodes[0]
		@tail = nodes.last
	end

	# Tries to allocate size contiguous free blocks, starting before before
	# Returns the index of the first block, or nil if unsuccessful
	def allocate(size, before)
		n = @head

		while n != nil && n.start < before
			if n.size >= size
				return allocate_from(n, size)
			end

			n = n.next
		end
	end

	# For testing
	def to_a
		a = []
		n = @head

		until n.nil?
			a.push([n.start, n.size])
			n = n.next
		end

		a
	end

	private

	# Precondition: node.size >= size
	def allocate_from(node, size)
		result = node.start

		if node.size == size
			unlink(node)
		else
			node.size -= size
			node.start += size
		end

		result
	end

	def unlink(node)
		if @head == node
			@head = node.next
		else
			node.prev.next = node.next
		end

		if @tail == node
			@tail = node.prev
		else
			node.next.prev = node.prev
		end
	end
end

FreeListNode = Struct.new(:prev, :next, :start, :size)

Day9File = Struct.new(:id, :blocks) do
	def checksum
		blocks.sum { |b| b * id }
	end
end

def defrag(fs)
	fs.files.each_index.reverse_each do |file_id|
		defrag_file(fs, file_id)
	end
end

def defrag_file(fs, file_id)
	f = fs.files[file_id]
	new_start_block = fs.free_list.allocate(f.blocks.length, f.blocks[0])

	if new_start_block.nil?
		return
	end

	f.blocks.length.times do |i|
		f.blocks[i] = new_start_block + i
	end

	# No need to return the old blocks to the free list. Because we defrag
	# files from right to left and only move files to the left, it will never
	# be used.
end


class Tests < Microtest::Test
	def test_filesystem_from_disk_map
		fs = Filesystem::from_disk_map("12345")
		assert_equal("0..111....22222".length, fs.nblocks, "nblocks")
		expected_free_list = [
			[1, 2],
			[6, 4]
		]
		assert_equal(expected_free_list, fs.free_list.to_a, "free_list")
		assert_equal(3, fs.files.length, "number of files")
		assert_equal([0], fs.files[0].blocks, "blocks of file 0")
		assert_equal([3, 4, 5], fs.files[1].blocks, "blocks of file 1")
		assert_equal([10, 11, 12, 13, 14], fs.files[2].blocks, "blocks of file 2")
	end

	def test_defrag_file
		fs = Filesystem::from_disk_map("2333133121414131402")
		defrag_file(fs, 9)
		assert_equal([2, 3], fs.files[9].blocks, "blocks of file 9")
	end

	def test_defrag
		fs = Filesystem::from_disk_map("2333133121414131402")
		defrag(fs)
		expected_file_blocks = [
			[0, 1],
			[5, 6, 7],
			[4],
			[15, 16, 17],
			[12, 13],
			[22, 23, 24, 25],
			[27, 28, 29, 30],
			[8, 9, 10],
			[36, 37, 38, 39],
			[2, 3]
		]
		assert_equal(expected_file_blocks, fs.files.map(&:blocks))
		expected_free_list = [[14, 1], [18, 1], [21, 1], [26, 1], [31, 1], [35, 1]]
		assert_equal(expected_free_list, fs.free_list.to_a)
	end
end


Microtest.run(Tests.new)

fs = Filesystem.from_disk_map(ARGF.read.chomp)
defrag(fs)
puts(fs.checksum)
