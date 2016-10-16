import Cocoa

class Circuit: NSObject {
	private let wires: PropertyDict = PropertyDict()
	
	func configure(line: String) {
		let words = line.components(separatedBy: " ")
		let arrow = words.index(of: "->")!
		
		if arrow == 1 {
			assign(name: words[2], operand: words[0])
		} else if arrow == 2 && words[0] == "NOT" {
			not(name: words[3], operand: words[1])
		} else if arrow == 3 && words[1] == "AND" {
			and(name: words[4], lhs: words[0], rhs: words[2])
		} else if arrow == 3 && words[1] == "OR" {
			or(name: words[4], lhs: words[0], rhs: words[2])
		} else if arrow == 3 && words[1] == "LSHIFT" {
			lshift(name: words[4], lhs: words[0], rhs: words[2])
		} else if arrow == 3 && words[1] == "RSHIFT" {
			rshift(name: words[4], lhs: words[0], rhs: words[2])
		}
	}

	func valueOf(wire: String) -> UInt16? {
		return wires.get(wire).value()
	}
	
	private func assign(name: String, operand: String) {
		let handle = wires.get(name)
		handle.prop = UnaryFunction(dependency: getOperand(operand),
		                            compute: { $0 })
	}

	private func not(name: String, operand: String) {
		let handle = wires.get(name)
		handle.prop = UnaryFunction(dependency: getOperand(operand),
		                            compute: { ~$0 })
	}

	private func and(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = BinaryFunction(lhs: getOperand(lhs), rhs: getOperand(rhs),
		                             compute: { $0 & $1 })
	}

	private func or(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = BinaryFunction(lhs: getOperand(lhs), rhs: getOperand(rhs),
		                             compute: { $0 | $1 })
	}

	private func lshift(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = BinaryFunction(lhs: getOperand(lhs), rhs: getOperand(rhs),
		                             compute: { $0 << $1 })
	}
	
	private func rshift(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = BinaryFunction(lhs: getOperand(lhs), rhs: getOperand(rhs),
		                             compute: { $0 >> $1 })
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
