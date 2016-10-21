//: Playground - noun: a place where people can play

import UIKit

var str = "Hello, playground"

let input = [[1, 2], [3]]
var args: [[Int]] = []
let x = input.flatMap { (a: [Int]) -> [[Int]] in
	args.append(a)
	return [a]
}
x
args

let y = [1, 2, 3].flatMap { [[$0, 5]] }
y