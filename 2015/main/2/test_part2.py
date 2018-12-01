import unittest
import sys
sys.path.append(".")
import part2

class TestPart2(unittest.TestCase):
	def test_ribbon_needed_for_one(self):
		r = part2.ribbon_needed_for_one(2, 3, 4)
		self.assertEqual(r, 34)
		r = part2.ribbon_needed_for_one(1, 1, 10)
		self.assertEqual(r, 14)

	def test_ribbon_needed(self):
		input = '''2x3x4
1x1x10
'''
		r = part2.ribbon_needed(input)
		self.assertEqual(r, 34 + 14)
