#!/usr/bin/env tclsh

proc fuelRequired mass {
	set ownFuel [expr $mass / 3 - 2]
	if {$ownFuel <= 0} {
		return 0
	} else {
		expr $ownFuel + [fuelRequired $ownFuel]
	}
}

set file [open "input.txt"]
set total 0

while {[gets $file line] >= 0} {
	set n [fuelRequired $line]
	set total [expr $total + $n]
}

puts $total
