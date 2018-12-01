Layer := Object clone
Layer range := 0
Layer scannerPos := 0
Layer dir := 1
Layer init := method(
	self range := 0
	self scannerPos := 0
	self dir := 1
)

Layer make := method(rng,
	layer := Layer clone
	layer range := rng
	layer
)

Layer advance := method(
	result := Layer make(self range)
	result dir = if(self mustReverse,
		self dir * -1,
		self dir
	)
	result scannerPos = self scannerPos + result dir
	result
)

Layer mustReverse := method(
	if(self dir == -1,
		self scannerPos == 0,
		self scannerPos == self range- 1
	)
)

Layer == := method(other,
	(other proto == self proto) and
		(other range == self range) and
		(other scannerPos == self scannerPos)
)

Layer != := method(other,
	(self == other) not
)

Layer asString := method(
	"Layer(r=" .. self range .. " s=" .. self scannerPos .. " dir=" .. self dir .. ")"
)
