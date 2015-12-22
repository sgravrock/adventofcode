import unittest
import sys
sys.path.append(".")
import part1

class TestPart1(unittest.TestCase):
	def test_has_straight(self):
		self.assertTrue(part1.has_straight("hijklmmn"))
		self.assertTrue(part1.has_straight("abc"))
		self.assertFalse(part1.has_straight("abd"))
		self.assertFalse(part1.has_straight("qbc"))
		self.assertFalse(part1.has_straight("yza"))

	def test_good_chars(self):
		self.assertTrue(part1.all_good_chars("hjkmmn"))
		self.assertFalse(part1.all_good_chars("hijklmmn"))

	def test_has_pairs(self):
		self.assertTrue(part1.has_pairs("abbceffg"))
		self.assertTrue(part1.has_pairs("bbff"))
		self.assertTrue(part1.has_pairs("abcdffaa"))
		self.assertFalse(part1.has_pairs("abbcegjk"))

	def test_is_valid(self):
		self.assertTrue(part1.is_valid("abcdffaa"))
		self.assertTrue(part1.is_valid("ghjaabcc"))
		self.assertFalse(part1.is_valid("hijklmmn"))
		self.assertFalse(part1.is_valid("abbceffg"))

	def test_string_sequence(self):
		seq = part1.string_sequence("azy")
		self.assertEqual(seq.next(), "azz")
		self.assertEqual(seq.next(), "baa")
		self.assertEqual(seq.next(), "bab")

	def test_nextpass(self):
		self.assertEqual(part1.nextpass("abcdefgh"), "abcdffaa")
		self.assertEqual(part1.nextpass("ghijklmn"), "ghjaabcc")
