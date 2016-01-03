import sys
import part1

def race(competitors, duration):
	scores = { c: 0 for c in competitors }
	for i in xrange(1, duration + 1):
		distances = [(c.distance(i), c) for c in competitors]
		for leader in find_leaders(distances):
			scores[leader] += 1
	return list(reversed(sorted([(scores[c], c.name) for c in scores])))
		

def find_leaders(distances):
	farthest = max([x[0] for x in distances])
	return [x[1] for x in distances if x[0] == farthest]


if __name__ == "__main__":
	results = race(part1.read_file(sys.stdin), int(sys.argv[1]))
	print results
