import sys

def compress_badly(s):
	entries = list()
	current = entry(None)
	for c in s:
		if current.c == c:
			current.n += 1
		else:
			merge(entries)
			current = entry(c)
			entries.append(current)
	return "".join([str(e.n) + e.c for e in entries])

def merge(entries):
	pass

class entry:
	def __init__(self, c):
		self.c = c
		self.n = 1


