import unittest
import sys
sys.path.append(".")
import part2

class TestPart1(unittest.TestCase):
	def test_naughty(self):
		self.assertFalse(part2.is_nice("uurcxstgmygtbstg"))
		self.assertFalse(part2.is_nice("eodomkazucvgmuy"))

	def test_nice(self):
		self.assertTrue(part2.is_nice("xxyxx"))
