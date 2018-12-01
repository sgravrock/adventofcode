import unittest
import sys
sys.path.append(".")
import part2

class TestPart2(unittest.TestCase):
	def test_num_houses_with_presents(self):
		self.assertEqual(part2.num_houses_with_presents('>v'), 3)
		self.assertEqual(part2.num_houses_with_presents('^>v<'), 3)
		self.assertEqual(part2.num_houses_with_presents('^v^v^v^v^v'), 11)
