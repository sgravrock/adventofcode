import unittest
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_num_houses_with_presents(self):
		self.assertEqual(part1.num_houses_with_presents('>'), 2)
		self.assertEqual(part1.num_houses_with_presents('^>v<'), 4)
		self.assertEqual(part1.num_houses_with_presents('^v^v^v^v^v'), 2)
