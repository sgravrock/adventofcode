import sys
import itertools

def readfile(f):
	result = {}
	for line in f:
		fields = line.rstrip().split(" ")
		p1 = fields[0]
		p2 = fields[10].replace(".", "")
		n = int(fields[3])
		if fields[2] == "lose":
			n *= -1
		result[(p1, p2)] = n
	return result

def optimal(config):
	diners = set([k[0] for k in config.keys()])
	arrangements = list(itertools.permutations(diners))
	all = [(arr, happiness(config, arr)) for arr in arrangements]
	return max(all, key=lambda p: p[1])

def happiness(config, arrangement):
	return sum([happiness_for_pair(config, p) for p in makepairs(arrangement)])

def happiness_for_pair(config, pair):
	opposite = (pair[1], pair[0])
	return config[pair] + config[opposite]

def makepairs(arr):
	n = len(arr)
	for i in xrange(1, n):
		yield (arr[i-1], arr[i])
	yield (arr[n-1], arr[0])

if __name__ == "__main__":
	print optimal(readfile(sys.stdin))
