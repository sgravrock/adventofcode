import Foundation

let input = [11,
             30,
             47,
             31,
             32,
             36,
             3,
             1,
             5,
             3,
             32,
             36,
             15,
             11,
             46,
             26,
             28,
             1,
             19,
             3
]
print("start at \(Date())")
let result = combinations(input, withSum: 150)
print(result.count)
print("end at \(Date())")
