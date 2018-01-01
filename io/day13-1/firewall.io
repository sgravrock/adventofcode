Firewall := Object clone
Firewall layers := list()
Firewall init := method(
	self layers := list()
)

Firewall make := method(parsedLines,
	fw := Firewall clone
	parsedLines foreach(i, line,
		pos := line at(0)
		depth := line at(1)

		while (fw layers size < pos,
			fw layers append(nil)
		)

		fw layers append(Layer make(depth))
	)
	fw
)

Firewall advance := method(
	result := Firewall clone
	result layers = layers map(l, if(l == nil, nil, l advance))
	result
)

Firewall == := method(other,
	if((self proto != other proto) or
			(self layers size != other layers size),
		return false
	)

	for(i, 0, self layers size,
		if(self layers at(i) != other layers at(i), return false)
	)

	true
)

Firewall != := method(other,
	(self == other) not
)

Firewall asString := method(
	"Firewall(" .. self layers .. ")"
)
