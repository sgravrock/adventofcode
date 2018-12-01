World := Object clone
World firewall := nil
World playerDepth := -1

World make := method(firewall,
	result := World clone
	result firewall := firewall
	result playerDepth := 0
	result
)

World currentSeverity := method(
	layer := self firewall layers at(self playerDepth)
	if(layer == nil, return 0)
	if(layer scannerPos == 0, playerDepth * layer range, 0)
)

World advance := method(
	result := World make(firewall advance)
	result playerDepth = playerDepth + 1
	result
)

World == := method(other,
	(self proto == other proto) and
		(self firewall == other firewall) and
		(self playerDepth == other playerDepth)
)

World != := method(other, (self == other) not)
