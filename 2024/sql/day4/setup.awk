{
	#for (i = 1; i <= NF; i++) print "insert into grid values (" i ", " NR ", '" $i '");"
	for (i = 1; i <= NF; i++) print "insert into grid values (" i ", " NR ", '" $i "');"
}

