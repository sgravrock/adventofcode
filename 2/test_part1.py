import unittest
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_paper_needed_for_one(self):
		a = part1.paper_needed_for_one(2, 3, 4)
		self.assertEqual(a, 58)
		a = part1.paper_needed_for_one(1, 1, 10)
		self.assertEqual(a, 43)

	def test_paper_needed(self):
		input = '''2x3x4
1x1x10
'''
		a = part1.paper_needed(input)
		self.assertEqual(a, 58 + 43)
