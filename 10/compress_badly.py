import sys

def compress_badly(s):
	final = finish_segment(reduce(reducer, s, [None, 0, ""]), None)
	return final[2]

def reducer(accum, nextval):
	if nextval == accum[0]:
		accum[1] = accum[1] + 1
	else:
		finish_segment(accum, nextval)
	return accum

def finish_segment(accum, nextval):
	if accum[0] is not None:
		accum[2] += str(accum[1]) + accum[0]
	accum[0] = nextval
	accum[1] = 1
	return accum
