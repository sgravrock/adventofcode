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
		handle.prop = Property(lhs: getOperand(operand), rhs: nil) { (value, _) -> UInt16? in
			return value
		}
	}

	private func not(name: String, operand: String) {
		let handle = wires.get(name)
		handle.prop = Property(lhs: getOperand(operand), rhs: nil) { (op, _) -> UInt16? in
			if let x = op {
				return ~x
			}
			
			return nil
		}
	}

	private func and(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = Property(lhs: getOperand(lhs), rhs: getOperand(rhs)) { (l, r) -> UInt16? in
			if let lv = l {
				if let rv = r {
					return lv & rv
				}
			}
			
			return nil
		}
	}

	private func or(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = Property(lhs: getOperand(lhs), rhs: getOperand(rhs)) { (l, r) -> UInt16? in
			if let lv = l {
				if let rv = r {
					return lv | rv
				}
			}
			
			return nil
		}
	}

	private func lshift(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = Property(lhs: getOperand(lhs), rhs: getOperand(rhs)) { (l, r) -> UInt16? in
			if let lv = l {
				if let rv = r {
					return lv << rv
				}
			}
			
			return nil
		}
	}
	
	private func rshift(name: String, lhs: String, rhs: String) {
		let handle = wires.get(name)
		handle.prop = Property(lhs: getOperand(lhs), rhs: getOperand(rhs)) { (l, r) -> UInt16? in
			if let lv = l {
				if let rv = r {
					return lv >> rv
				}
			}
			
			return nil
		}
	}
	
	private func getOperand(_ name: String) -> PropertyHandle {
		if let n = UInt16(name) {
			// todo: a constant class would work well here
			let p = Property(lhs: nil, rhs: nil, compute: { (_, _) -> UInt16? in
				return n
			})
			return PropertyHandle(prop: p)
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

typealias BinaryComputeFunc = (UInt16?, UInt16?) -> UInt16?

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
		
		let maybeValue = prop?.value()
		
		if let v = maybeValue {
			memoizedValue = v
			prop = nil
		}
		
		return maybeValue
	}
}


class Property {
	private let lhsDependency: PropertyHandle?
	private let rhsDependency: PropertyHandle?
	private let computeFunc: BinaryComputeFunc
	
	
	init(lhs: PropertyHandle?,
	     rhs: PropertyHandle?,
	     compute: @escaping BinaryComputeFunc) {
		
		lhsDependency = lhs
		rhsDependency = rhs
		computeFunc = compute
	}
	
	func value() -> UInt16? {
		return computeFunc(lhsDependency?.value(), rhsDependency?.value())

	}
}
