import sys
import json

def sum_numbers(s):
	doc = json.loads(s)
	return deep_sum(doc)

def deep_sum(doc):
	if isinstance(doc, int):
		return doc
	elif isinstance(doc, dict):
		if u"red" in doc.values():
			return 0
		else:
			return sum([deep_sum(doc[k]) for k in doc])
	elif isinstance(doc, str) or isinstance(doc, unicode):
		return 0
	else:
		return sum([deep_sum(x) for x in doc])

if __name__ == "__main__":
	print deep_sum(json.load(sys.stdin))
