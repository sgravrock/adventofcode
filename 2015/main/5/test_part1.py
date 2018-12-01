import unittest
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_naughty(self):
		self.assertFalse(part1.is_nice("dvszwmarrgswjxmb"))
		self.assertFalse(part1.is_nice("jchzalrnumimnmhp"))
		self.assertFalse(part1.is_nice("haegwjzuvuyypxyu"))
		self.assertFalse(part1.is_nice("dvszwmarrgswjxmb"))

	def test_nice(self):
		self.assertTrue(part1.is_nice("ugknbfddgicrmopn"))
		self.assertTrue(part1.is_nice("aaa"))
