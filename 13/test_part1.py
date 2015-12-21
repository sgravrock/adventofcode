import unittest
import sys
sys.path.append(".")
import part1
from StringIO import StringIO

class TestPart1(unittest.TestCase):
	def test_readfile(self):
		f = StringIO("""David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.""")
		r = part1.readfile(f)
		self.assertEqual(r, {("David", "Bob"): -7, ("David", "Carol"): 41})

	def test_makepairs(self):
		arrangement = ["David", "Alice", "Carol"]
		expected = set([("David", "Alice"), ("Alice", "Carol"), ("Carol", "David")])
		self.assertEqual(set(part1.makepairs(arrangement)), expected)

	def test_happiness(self):
		with open("test_input") as f:
			config = part1.readfile(f)
		arrangement = ["David", "Alice", "Bob", "Carol"]
		self.assertEqual(part1.happiness(config, arrangement), 330)
		# And the reverse order should give the same result.
		arrangement = ["Carol", "Bob", "Alice", "David"]
		self.assertEqual(part1.happiness(config, arrangement), 330)
