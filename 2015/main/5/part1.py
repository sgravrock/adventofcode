import sys

def is_nice(s):
	return has_vowels(s, 3) and has_double(s) and not has_banned(s)

def num_nice(input):
	return len([s for s in input.split('\n') if is_nice(s)])

def has_vowels(s, nreq):
	return len([c for c in s if c in "aeiou"]) >= nreq

def has_double(s):
	for i in xrange(1, len(s)):
		if s[i - 1] == s[i]:
			return True
	return False

def has_banned(s):
	return len([b for b in ["ab", "cd", "pq", "xy"] if b in s]) > 0

if __name__ == "__main__":
	print num_nice(sys.stdin.read())
