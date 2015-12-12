import unittest
import sys
sys.path.append(".")
from compress_badly import compress_badly

class TestPart1(unittest.TestCase):
	def test_compress_badly(self):
		self.assertEqual("11", compress_badly("1"))
		self.assertEqual("21", compress_badly("11"))
		self.assertEqual("1211", compress_badly("21"))
		self.assertEqual("312211", compress_badly("111221"))
