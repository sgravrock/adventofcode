import Foundation

let s = try String(contentsOfFile:"../../day12_1/input.json", encoding:.utf8)
let n = try sumNumbers(json: s)
print(n)
