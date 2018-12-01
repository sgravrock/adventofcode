import sys

def num_nice(input):
	return len([s for s in input.split('\n') if is_nice(s)])

def is_nice(s):
	return has_non_overlapping_pairs(s) and has_repeat_with_gap(s)

def has_non_overlapping_pairs(s):
	for i in xrange(0, len(s) - 3):
		needle = s[i:i+2]
		haystack = s[i+2:]
		if needle in haystack:
			return True
	return False

def has_repeat_with_gap(s):
	for i in xrange(0, len(s) - 2):
		if s[i + 2] == s[i]:
			return True
	return False

if __name__ == "__main__":
	print num_nice(sys.stdin.read())
