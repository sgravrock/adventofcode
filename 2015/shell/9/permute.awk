BEGIN { nitems = 0 }
{ items[nitems++] = $1 }
END { permute(items, nitems) }

function permute(items, len, prefix, prefixlen, i, j, subitems, nsub) {
	if (prefixlen == "") { prefixlen = 0 }
	if (len == 0) {
		for (i = 0; i < prefixlen; i++) {
			printf("%s ", prefix[i]);
		}

		print ""
		return
	}

	for (i = 0; i < len; i++) {
		prefix[prefixlen] = items[i]
		nsub = 0

		for (j = 0; j < len; j++) {
			if (j != i) {
				subitems[nsub++] = items[j];
			}
		}

		permute(subitems, nsub, prefix, prefixlen + 1)
	}
}
