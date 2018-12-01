import Foundation

let s = try String(contentsOfFile:"../../day8_1/input.txt", encoding:.utf8)
let lines = s.components(separatedBy: "\n")
print(try delta(lines: lines))
