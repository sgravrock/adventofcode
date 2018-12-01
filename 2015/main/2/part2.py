import sys

def ribbon_needed(input):
	lines = input.split('\n')
	packages = [[int(d) for d in line.split('x')] for line in lines if line != '']
	return sum([ribbon_needed_for_one(p[0], p[1], p[2]) for p in packages])


def ribbon_needed_for_one(w, l, h):
	faces = [[w, l], [w, h], [l, h]]
	wrap = min([perimeter(f) for f in faces])
	volume = w * l * h
	return wrap + volume

def perimeter(f):
	return 2 * (f[0] + f[1])

if __name__ == "__main__":
	print ribbon_needed(sys.stdin.read())
