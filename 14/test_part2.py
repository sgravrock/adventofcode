import unittest
from StringIO import StringIO
import sys
sys.path.append(".")
import part1
import part2

class TestPart1(unittest.TestCase):
	def test_race(self):
		comet = part1.reindeer("Comet", 14, 10, 127)
		dancer = part1.reindeer("Dancer", 16, 11, 162)
		prancer = part1.reindeer("Prancer", 16, 11, 162)
		self.assertEqual(part2.race([comet, dancer], 1), [(1, "Dancer"), (0, "Comet")])
		tie = part2.race([prancer, dancer], 1)
		self.assertEqual(1, tie[0][0])
		self.assertEqual(1, tie[1][0])
		self.assertEqual(part2.race([comet, dancer], 140), [(139, "Dancer"), (1, "Comet")])
		self.assertEqual(part2.race([comet, dancer], 1000), [(689, "Dancer"), (312, "Comet")])
