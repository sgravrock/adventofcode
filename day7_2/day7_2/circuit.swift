import Cocoa

class Circuit: NSObject {
	private let wires: PropertyDict = PropertyDict()
	
	func configure(line: String) {
		let words = line.components(separatedBy: " ")
		let arrow = words.index(of: "->")!
		
		if arrow == 1 {
			unary(name: words[2], operand: words[0], compute: { $0 })
		} else if arrow == 2 && words[0] == "NOT" {
			unary(name: words[3], operand: words[1], compute: { ~$0 })
		} else if arrow == 3 && words[1] == "AND" {
			binary(name: words[4], lhs: words[0], rhs: words[2], compute: { $0 & $1 })
		} else if arrow == 3 && words[1] == "OR" {
			binary(name: words[4], lhs: words[0], rhs: words[2], compute: { $0 | $1 })
		} else if arrow == 3 && words[1] == "LSHIFT" {
			binary(name: words[4], lhs: words[0], rhs: words[2], compute: { $0 << $1 })
		} else if arrow == 3 && words[1] == "RSHIFT" {
			binary(name: words[4], lhs: words[0], rhs: words[2], compute: { $0 >> $1 })
		}
	}
	
	func valueOf(wire: String) -> UInt16? {
		return wires.get(wire).value()
	}
	
	private func unary(name: String, operand: String, compute: @escaping UnaryComputeFunc) {
		let handle = wires.get(name)
		handle.prop = UnaryFunction(dependency: getOperand(operand), compute: compute)
	}
	
	private func binary(name: String, lhs: String, rhs: String, compute: @escaping BinaryComputeFunc) {
		let handle = wires.get(name)
		handle.prop = BinaryFunction(lhs: getOperand(lhs), rhs: getOperand(rhs), compute: compute)
	}
	
	private func getOperand(_ name: String) -> PropertyHandle {
		if let n = UInt16(name) {
			return PropertyHandle(prop: Constant(value: n))
		}
		
		return wires.get(name)
	}
}

class PropertyDict {
	private var storage: Dictionary<String, PropertyHandle> = [:]
	
	func get(_ name: String) -> PropertyHandle {
		if let handle = storage[name] {
			return handle
		}
		
		storage[name] = PropertyHandle(prop: nil)
		return storage[name]!
	}
}

typealias UnaryComputeFunc = (UInt16) -> UInt16
typealias BinaryComputeFunc = (UInt16, UInt16) -> UInt16

class PropertyHandle {
	var prop: Property?
	private var memoizedValue: UInt16?
	
	init(prop: Property?) {
		self.prop = prop
	}
	
	func value() -> UInt16? {
		if let v = memoizedValue {
			return v
		}
		
		if !(prop?.ready() ?? false) {
			return nil
		}
		
		memoizedValue = prop!.value()
		prop = nil
		return memoizedValue
	}
}

protocol Property {
	func value() -> UInt16
	func ready() -> Bool
}

class Constant : Property {
	private let v: UInt16
	
	init(value: UInt16) {
		v = value
	}
	
	func ready() -> Bool {
		return true
	}
	
	func value() -> UInt16 {
		return v
	}
}

class UnaryFunction : Property {
	private let dependency: PropertyHandle
	private let computeFunc: UnaryComputeFunc
	
	init(dependency: PropertyHandle, compute: @escaping UnaryComputeFunc) {
		self.dependency = dependency
		computeFunc = compute
	}
	
	func ready() -> Bool {
		return dependency.value() != nil
	}
	
	func value() -> UInt16 {
		return computeFunc(dependency.value()!)
	}
}

class BinaryFunction : Property {
	private let lhsDependency: PropertyHandle
	private let rhsDependency: PropertyHandle
	private let computeFunc: BinaryComputeFunc
	
	
	init(lhs: PropertyHandle,
	     rhs: PropertyHandle,
	     compute: @escaping BinaryComputeFunc) {
		
		lhsDependency = lhs
		rhsDependency = rhs
		computeFunc = compute
	}
	
	func ready() -> Bool {
		return lhsDependency.value() != nil && rhsDependency.value() != nil
	}
	
	func value() -> UInt16 {
		return computeFunc(lhsDependency.value()!, rhsDependency.value()!)
	}
}
