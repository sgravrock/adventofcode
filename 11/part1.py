from __future__ import generators
import re

def nextpass(p):
	for candidate in string_sequence(p):
		if is_valid(candidate):
			return candidate

def is_valid(p):
	return has_straight(p) and all_good_chars(p) and has_pairs(p)

def has_straight(p):
	for i in xrange(0, len(p) - 2):
		if is_straight(p, i):
			return True
	return False

def is_straight(p, start):
	for i in xrange(start + 1, start + 3):
		if p[i - 1] == 'z' or p[i] != nextchar(p[i-1]):
			return False
	return True

def nextchar(c):
	if c == 'z':
		return 'a'
	else:
		return chr(ord(c) + 1)

def all_good_chars(p):
	for c in "iol":
		if c in p:
			return False
	return True

def has_pairs(p):
	return re.search(r"(.)\1.*(.)\2", p) != None

def string_sequence(s):
	while True:
		s = nextstring(s)
		yield s

def nextstring(s):
	sl = list(s)

	for i in xrange(len(sl) - 1, -1, -1):
		if sl[i] != 'z':
			sl[i] = nextchar(sl[i])
			return "".join(sl)
		else:
			for j in xrange(i, len(sl)):
				sl[i] = 'a'

if __name__ == "__main__":
	print nextpass("hepxcrrq")
