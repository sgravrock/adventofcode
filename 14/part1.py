import sys

class reindeer:
	def __init__(self, name, speed, endurance, rest):
		self.name = name
		self.speed = speed
		self.endurance = endurance
		self.rest = rest
	def __str__(self):
		return self.name
	def distance(self, seconds):
		total = 0
		elapsed = 0
		while elapsed < seconds:
			d = min(seconds - elapsed, self.endurance)
			total += d * self.speed
			elapsed += d + self.rest
		return total

def read_file(f):
	return [read_line(line) for line in f]

def read_line(line):
	tokens = line.split(" ")
	return reindeer(tokens[0], int(tokens[3]), int(tokens[6]), int(tokens[13]))

def race(competitors, duration):
	return max([(c.distance(duration), c) for c in competitors])


if __name__ == "__main__":
	winner = race(read_file(sys.stdin), int(sys.argv[1]))
	print winner[1].name, winner[0]
