import sys

def num_houses_with_presents(input):
	santas = [(0, 0), (0, 0)]
	deliveries = {(0, 0): 2}
	for i in xrange(0, len(input)):
		which = i % 2
		santas[which] = move(santas[which], input[i])
		deliveries[santas[which]] = deliveries.get(santas[which], 0) + 1
	return len(deliveries)

def move(pos, c):
	if c == '>':
		return (pos[0] + 1, pos[1])
	elif c == '<':
		return (pos[0] - 1, pos[1])
	elif c == '^':
		return (pos[0], pos[1] - 1)
	elif c == 'v':
		return (pos[0], pos[1] + 1)
	else:
		raise Error("Don't know how to " + c)

if __name__ == "__main__":
	print num_houses_with_presents(sys.stdin.read())
