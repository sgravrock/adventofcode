/^[0-9]+ ->/ {
	values[$3] = $1
	print
}

/^[a-zA-Z]+ ->/ {
	maybe_sub()
}

/ AND / {
	if (isnum($1) && isnum($3)) {
		print and($1, $3) " -> " $5
	} else {
		maybe_sub()
	}
}

/ OR / {
	if (isnum($1) && isnum($3)) {
		print or($1, $3) " -> " $5
	} else {
		maybe_sub()
	}
}

/ LSHIFT / {
	if (isnum($1) && isnum($3)) {
		print lshift($1, $3) " -> " $5
	} else {
		maybe_sub()
	}
}

/ RSHIFT / {
	if (isnum($1) && isnum($3)) {
		print rshift($1, $3) " -> " $5
	} else {
		maybe_sub()
	}
}

/^NOT / {
	if (isnum($2)) {
	  print and(0xFFFF, compl($2)) " -> " $4
	} else {
		maybe_sub()
	}
}

function isnum(x) {
	return x ~ /^[0-9]+$/;
}

function maybe_sub(i) {
	for (i = 1; i <= NF; i++) {
		if ($i in values) {
			$i = values[$i]
		}
	}

	print
}
