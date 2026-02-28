//
//  TaskType.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Foundation

enum TaskType: CaseIterable {
    case simple
    case test1, test2, test3, test4, test5, test6
    case allTests

    /// The 1-based test index, or nil for non-individual-test types.
    var testIndex: Int? {
        switch self {
        case .test1: return 1
        case .test2: return 2
        case .test3: return 3
        case .test4: return 4
        case .test5: return 5
        case .test6: return 6
        default: return nil
        }
    }

    /// All individual test cases.
    static var allTestCases: [TaskType] {
        [.test1, .test2, .test3, .test4, .test5, .test6]
    }
}
