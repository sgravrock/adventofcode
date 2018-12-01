//
//  day1_2_tests.swift
//  day1_2_tests
//
//  Created by Steve Gravrock on 10/15/16.
//  Copyright © 2016 Steve Gravrock. All rights reserved.
//

import XCTest
//@testable import day1_2

class day1_2_tests: XCTestCase {
	func testNavigate() {
		XCTAssertEqual(1, navigate(toFloor: -1, directions: ")"))
		XCTAssertEqual(5, navigate(toFloor: -1, directions: "()())"))
	}
}
