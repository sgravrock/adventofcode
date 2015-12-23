import unittest
from StringIO import StringIO
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_read_file(self):
		result = part1.read_file(StringIO("""Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."""))
		self.assertEqual(result[0].speed, 14)
		self.assertEqual(result[0].endurance, 10)
		self.assertEqual(result[0].rest, 127)
		self.assertEqual(result[1].speed, 16)
		self.assertEqual(result[1].endurance, 11)
		self.assertEqual(result[1].rest, 162)

	def test_reindeer_distance(self):
		comet = part1.reindeer("Comet", 14, 10, 127)
		self.assertEqual(comet.distance(1), 14)
		self.assertEqual(comet.distance(10), 140)
		self.assertEqual(comet.distance(11), 140) # resting
		self.assertEqual(comet.distance(137), 140) # resting
		self.assertEqual(comet.distance(138), 154)
		self.assertEqual(comet.distance(140), 182)

	def test_race(self):
		comet = part1.reindeer("Comet", 14, 10, 127)
		dancer = part1.reindeer("Dancer", 16, 11, 162)
		self.assertEqual(part1.race([comet, dancer], 1000), (1120, comet))
