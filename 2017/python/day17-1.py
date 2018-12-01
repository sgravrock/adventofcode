#!/usr/bin/env python3
import pytest

class RingBuffer:
    def __init__(self, initial_data):
        assert(len(initial_data) > 0)
        self.data = initial_data[:]
        self.pos = 0

    def current(self):
        return self.data[self.pos]

    def advance(self, n):
        self.pos = (self.pos + n) % len(self.data)

    def insert(self, value):
        self.pos += 1
        self.data.insert(self.pos, value)

    def describe(self):
        return ' '.join([self.describe_one(x, i) for i, x in enumerate(self.data)])

    def describe_one(self, value, pos):
        fmt = '({})' if pos == self.pos else '{}'
        return fmt.format(value)

    
def test_RingBuffer_advance():
    subject = RingBuffer([1, 2, 3])
    assert subject.current() == 1
    subject.advance(1)
    assert subject.current() == 2
    subject.advance(1)
    assert subject.current() == 3
    subject.advance(1)
    assert subject.current() == 1
    subject.advance(2)
    assert subject.current() == 3

def test_RingBuffer_insert():
    subject = RingBuffer([0])
    subject.insert(1)
    assert subject.describe() == '0 (1)'
    subject.advance(1)
    assert subject.describe() == '(0) 1'
    subject.insert(2)
    assert subject.describe() == '0 (2) 1'


class Spinlock:
    def __init__(self, steps_per_insert):
        self.buf = RingBuffer([0])
        self.steps_per_insert = steps_per_insert
        self.nextval = 1

    def insert(self):
        self.buf.advance(self.steps_per_insert)
        self.buf.insert(self.nextval)
        self.nextval += 1


def test_Spinlock_insert():
    subject = Spinlock(3)
    assert subject.buf.describe() == '(0)'
    subject.insert()
    assert subject.buf.describe() == '0 (1)'
    subject.insert()
    assert subject.buf.describe() == '0 (2) 1'
    subject.insert()
    assert subject.buf.describe() == '0 2 (3) 1'
    subject.insert()
    assert subject.buf.describe() == '0 2 (4) 3 1'
    subject.insert()
    assert subject.buf.describe() == '0 (5) 2 4 3 1'
    subject.insert()
    assert subject.buf.describe() == '0 5 2 4 3 (6) 1'
    subject.insert()
    assert subject.buf.describe() == '0 5 (7) 2 4 3 6 1'
    subject.insert()
    assert subject.buf.describe() == '0 5 7 2 4 3 (8) 6 1'
    subject.insert()
    assert subject.buf.describe() == '0 (9) 5 7 2 4 3 8 6 1'


def value_after_n(n, steps):
    spinlock = Spinlock(steps)
    for _ in range(0, n):
        spinlock.insert()
    spinlock.buf.advance(1) # Encapsuwhatnow?
    return spinlock.buf.current()

def test_value_after_n():
    assert value_after_n(2017, 3) == 638

if __name__ == "__main__":
    if pytest.main([__file__]) != 0:
        exit(1)

    print(value_after_n(2017, 370))
