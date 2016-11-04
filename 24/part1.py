#!/usr/bin/env python
import itertools

def minQE(packages):
    weight = sum(packages) / 3
    for i in xrange(1, len(packages) - 2):
        m = minQE2(packages, weight, i)
        if m is not None:
            return m
    return None

def minQE2(packages, weight, passengerCount):
    allCombos = itertools.combinations(packages, passengerCount)
    combosWithMinWeight = (x for x in allCombos if sum(x) == weight)
    sortedByQE = sorted(combosWithMinWeight, key=quantumEntanglement)
    for combo in sortedByQE:
        if isValid(packages, weight, combo):
            return quantumEntanglement(combo)
    return None

def isValid(packages, weight, passengerCombo):
    notInPassenger = [x for x in packages if x not in passengerCombo]
    for combo in combinationsWithSum(notInPassenger, weight):
        rest = (x for x in notInPassenger if x not in combo)
        if sum(rest) == weight:
            return True
    return False

def quantumEntanglement(packages):
    qe = 1
    for p in packages:
        qe *= p
    return qe

def combinations(input):
    for i in xrange(1, len(input) + 1):
        for r in itertools.combinations(input, i):
            yield r

def combinationsWithSum(input, desiredSum):
    combos = combinations(input)
    return (x for x in combos if sum(x) == desiredSum)

if __name__ == "__main__":
    packages = [
        1,
        2,
        3,
        5,
        7,
        13,
        17,
        19,
        23,
        29,
        31,
        37,
        41,
        43,
        53,
        59,
        61,
        67,
        71,
        73,
        79,
        83,
        89,
        97,
        101,
        103,
        107,
        109,
        113
    ]
    print minQE(packages)
