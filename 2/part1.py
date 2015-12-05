import sys

def paper_needed(input):
	lines = input.split('\n')
	packages = [[int(d) for d in line.split('x')] for line in lines if line != '']
	return sum([paper_needed_for_one(p[0], p[1], p[2]) for p in packages])

def paper_needed_for_one(w, l, h):
	faces = [w*l, w*h, l*h]
	return 2 * sum(faces) + min(faces)

if __name__ == "__main__":
	print paper_needed(sys.stdin.read())
