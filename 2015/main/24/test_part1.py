import unittest
import sys
import itertools
sys.path.append(".")
from part1 import minQE, combinations, combinationsWithSum, isValid

class TestPart1(unittest.TestCase):
    def test_minQE(self):
        result = minQE([1, 2, 3, 4, 5, 7, 8, 9, 10, 11])
        self.assertEqual(99, result)

    def test_combinations(self):
        expected = [
            (3, 4, 5),
            (3, 4),
            (3, 5),
            (4, 5),
            (3,),
            (4,),
            (5,)
        ]
        actual = list(combinations([3, 4, 5]))
        self.assertEqual(len(expected), len(actual))
        for e in expected:
            self.assertTrue(e in actual, "Didn't find {0}".format(e))

    def test_combinationsWithSum(self):
        things = [20, 15, 10, 5, 5]
        expected = [
            (15, 10),
            (20, 5),
            (20, 5),
            (15, 5, 5)
        ]
        actual = list(combinationsWithSum(things, 25))
        self.assertEqual(len(expected), len(actual))
        for e in expected:
            self.assertTrue(e in actual, "Didn't find {0}".format(e))

    def test_isValid(self):
        things = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11]
        self.assertTrue(isValid(things, 20, [11, 9]))
