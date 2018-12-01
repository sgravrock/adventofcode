import Foundation

let input = [
	50,
	44,
	11,
	49,
	42,
	46,
	18,
	32,
	26,
	40,
	21,
	7,
	18,
	43,
	10,
	47,
	36,
	24,
	22,
	40
]
print("start at \(Date())")
let result = minimalCombinations(input, withSum: 150)
print(result.count)
print("end at \(Date())")
