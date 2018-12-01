import sys

def num_houses_with_presents(input):
	x = 0
	y = 0
	deliveries = {(x, y): 1}
	for c in input:
		if c == '>':
			x += 1
		elif c == '<':
			x -= 1
		elif c == '^':
			y -= 1
		elif c == 'v':
			y += 1
		else:
			raise Error("Don't know how to " + c)
		where = (x, y)
		deliveries[where] = deliveries.get(where, 0) + 1
	return len(deliveries)

if __name__ == "__main__":
	print num_houses_with_presents(sys.stdin.read())
