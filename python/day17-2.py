#!/usr/bin/env python3
import pytest

class Spinlock:
    def __init__(self, steps_per_insert):
        self.pos = 0
        self.value1 = None
        self.steps_per_insert = steps_per_insert
        self.nextval = 1

    def insert(self):
        self.pos = ((self.pos + self.steps_per_insert) % self.nextval) + 1
        if self.pos == 1:
            self.value1 = self.nextval
        self.nextval += 1


def test_Spinlock_insert():
    subject = Spinlock(3)
    assert subject.value1 is None
    subject.insert()
    assert subject.value1 == 1 # 0 (1)
    subject.insert()
    assert subject.value1 == 2 # 0 (2) 1
    subject.insert()
    assert subject.value1 == 2 # 0 2 (3) 1
    subject.insert()
    assert subject.value1 == 2 # 0 2 (4) 3 1
    subject.insert()
    assert subject.value1 == 5 # 0 (5) 2 4 3 1
    subject.insert()
    assert subject.value1 == 5 # 0 5 2 4 3 (6) 1
    subject.insert()
    assert subject.value1 == 5 # 0 5 (7) 2 4 3 6 1
    subject.insert()
    assert subject.value1 == 5 # 0 5 7 2 4 3 (8) 6 1
    subject.insert()
    assert subject.value1 == 9 # 0 (9) 5 7 2 4 3 8 6 1


def value1(n_inserts, steps):
    spinlock = Spinlock(steps)
    for i in range(0, n_inserts):
        if i % 100000 == 0:
            print("iteration {} ({}%)".format(i, (i / n_inserts) * 100));
        spinlock.insert()
    return spinlock.value1

def test_value1():
    assert value1(2017, 3) == 1226

if __name__ == "__main__":
    if pytest.main([__file__]) != 0:
        exit(1)

    print(value1(50000000, 370))
