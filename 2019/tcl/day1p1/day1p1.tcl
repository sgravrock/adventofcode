#!/usr/bin/env tclsh

set file [open "input.txt"]
set total 0

while {[gets $file line] >= 0} {
	set n [expr $line / 3 - 2]
	set total [expr $total + $n]
}

puts $total
