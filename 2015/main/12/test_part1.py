import unittest
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_sum_numbers_1(self):
		self.assertEqual(part1.sum_numbers("[]"), 0)
	def test_sum_numbers_2(self):
		self.assertEqual(part1.sum_numbers("{}"), 0)
	def test_sum_numbers_3(self):
		self.assertEqual(part1.sum_numbers("[1,2,3]"), 6)
	def test_sum_numbers_4(self):
		self.assertEqual(part1.sum_numbers("{\"a\":2,\"b\":4}"), 6)
	def test_sum_numbers_5(self):
		self.assertEqual(part1.sum_numbers("[[[3]]]"), 3)
	def test_sum_numbers_6(self):
		self.assertEqual(part1.sum_numbers("{\"a\":{\"b\":4},\"c\":-1}"), 3)
	def test_sum_numbers_7(self):
		self.assertEqual(part1.sum_numbers("[-1,{\"a\":1}]"), 0)
	def test_sum_numbers_8(self):
		self.assertEqual(part1.sum_numbers("{\"a\":[-1,1]}"), 0)
