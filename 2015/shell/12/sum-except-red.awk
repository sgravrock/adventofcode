# Recognizes a restricted set of JSON that:
# * Contains no numbers in strings, e.g "42" or "I have 42 things"
# * Contains no escaped quotes in strings
# * Has had each token split onto each line, as by the tokenize script
# * Contains no piles of poo (💩)

# Requires GNU awk.
# Usual usage: ./tokenize < input-file | gawk -f parse.awk 

BEGIN {
	idprefix = "💩"
	statekey = idprefix idprefix "state"
	depth = 0
	lastid = 0
	ids[0] = "ROOT"
	state[0] = "array" # Allows us to not treat the top level specially
	data["ROOT"][statekey] = state[0]
	lengths["ROOT"] = 0
}
END { print sum_except_red("ROOT") }

/\[/ {
	enter("array")
	lengths[ids[depth]] = 0
}


/{/ { enter("object") }

/[\]}]/ { depth-- }

/[0-9\-]+/ {
	if (state[depth] == "array") {
		setval($1)
	} else if (state[depth] == "key") {
		setval($1)
	} else {
		die("Got a number (" $1 ") when in an invalid state: " state[depth])
	}
}

/"[^"]+"/ {
	if (state[depth] == "array") {
		setval($1)
	} else if (state[depth] == "object") {
		state[depth] = "key"
		key[depth] = $1
	} else if (state[depth] == "key") {
		setval($1)
	} else {
		die("Got a string when in an invalid state: " state[depth])
	}
}

function enter(newstate, nextid) {
	nextid = idprefix (lastid + 1)
	setval(nextid)
	lastid++
	depth++
	state[depth] = newstate
	ids[depth] = lastid
	data[lastid][statekey] = newstate
}

function setval(v, id, a, k) {
	if (state[depth] == "array") {
		k = lengths[ids[depth]]++
	} else if (state[depth] == "key") {
		k = key[depth]
		state[depth] = "object"
	} else {
		die("setval() when in an invalid state: " state[depth])
	}

	id = ids[depth]
	data[id][k] = v
}

function sum_except_red(id, k, i, n) {
	n = 0

	if (data[id][statekey] == "array") {
		for (i = 0; i < lengths[id]; i++) {
			n += sum_value_except_red(data[id][i])
		}
	} else if (data[id][statekey] == "object") {
		for (k in data[id]) {
			if (data[id][k] == "\"red\"") {
				return 0
			} else if (k != statekey) {
				n += sum_value_except_red(data[id][k]);
			}
		}
	} else {
		die("Invalid state: " data[id][statekey])
	}

	return n
}

function sum_value_except_red(value, descendent, pattern) {
	pattern = "^" idprefix

	if (value ~ pattern) {
		descendent = value
		sub(pattern, "", descendent)
		return sum_except_red(descendent)
	} else if (isnum(value)) {
		return value
	} else {
		return 0
	}
}

function die(msg) {
	print msg | "cat 1>&2"
	exit 1
}

function isnum(x) {
	return x ~ /^[0-9\-]+$/;
}

