import unittest
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_compress_badly(self):
		self.assertEqual("11", part1.compress_badly("1"))
		self.assertEqual("21", part1.compress_badly("11"))
		self.assertEqual("1211", part1.compress_badly("21"))
		self.assertEqual("312211", part1.compress_badly("111221"))
