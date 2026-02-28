//
//  BarlimanModel.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import SwiftUI

// MARK: - Task Status

/// Represents the current state of a query task (definition check, individual test, or all-tests).
enum TaskStatus: Equatable {
    case idle
    case thinking
    case succeeded(elapsed: TimeInterval)
    case failed(elapsed: TimeInterval)
    case syntaxError
    case parseError
    case evaluationFailed

    var isSpinning: Bool {
        self == .thinking
    }

    var label: String {
        switch self {
        case .idle: return ""
        case .thinking: return "???"
        case .succeeded(let t): return String(format: "Succeeded (%.2f s)", t)
        case .failed(let t): return String(format: "Failed (%.2f s)", t)
        case .syntaxError: return "Illegal sexpression"
        case .parseError: return "Syntax error"
        case .evaluationFailed: return "Evaluation failed"
        }
    }

    var color: Color {
        switch self {
        case .idle: return .primary
        case .thinking: return Color(.systemPurple)
        case .succeeded: return .primary
        case .failed: return Color(.systemRed)
        case .syntaxError: return Color(.systemOrange)
        case .parseError: return Color(.systemPink)
        case .evaluationFailed: return Color(.systemRed)
        }
    }

    var nsColor: NSColor {
        switch self {
        case .idle: return .textColor
        case .thinking: return .systemPurple
        case .succeeded: return .textColor
        case .failed: return .systemRed
        case .syntaxError: return .systemOrange
        case .parseError: return .systemPink
        case .evaluationFailed: return .systemRed
        }
    }
}

// MARK: - Test State

/// Observable state for a single test slot (1 of 6).
@Observable
class TestState: Identifiable {
    let id: Int  // 1-based index (1...6)
    var input: String = ""
    var expectedOutput: String = ""
    var status: TaskStatus = .idle

    var isActive: Bool {
        !input.isEmpty && !expectedOutput.isEmpty
    }

    init(id: Int) {
        self.id = id
    }
}

// MARK: - Barliman Model

/// The central observable model for the Barliman app.
/// Holds all UI state; the SchemeEngine reads from and writes to this model.
@Observable
class BarlimanModel {
    // Definition pane
    var definitionText: String = "(define ,A\n  (lambda ,B\n    ,C))"
    var definitionStatus: TaskStatus = .idle

    // Best guess pane
    var bestGuessText: String = ""
    var bestGuessStatus: TaskStatus = .idle

    // 6 test groups
    var tests: [TestState] = (1...6).map { TestState(id: $0) }

    // Engine (set after init to break circular dependency)
    var engine: SchemeEngine?

    /// Called whenever any text field changes (definition or test inputs/outputs).
    func textDidChange() {
        engine?.onTextChanged()
    }

    /// Updates the result for a given task type based on process output.
    func updateResult(for taskType: TaskType, exitStatus: Int32, output: String, elapsed: TimeInterval) {
        switch taskType {
        case .simple:
            updateSimpleResult(exitStatus: exitStatus, output: output)
        case .test1, .test2, .test3, .test4, .test5, .test6:
            if let idx = taskType.testIndex {
                updateTestResult(index: idx, exitStatus: exitStatus, output: output, elapsed: elapsed)
            }
        case .allTests:
            updateAllTestsResult(exitStatus: exitStatus, output: output, elapsed: elapsed)
        }
    }

    // MARK: - Result Parsing

    private func updateSimpleResult(exitStatus: Int32, output: String) {
        if exitStatus == 0 {
            if output == "parse-error-in-defn" {
                definitionStatus = .parseError
            } else if output == "illegal-sexp-in-defn" {
                definitionStatus = .syntaxError
            } else if output == "()" {
                definitionStatus = .evaluationFailed
                engine?.cancelAllTestsOperation()
            } else {
                definitionStatus = .idle
            }
        } else if exitStatus == 15 {
            // SIGTERM — operation was cancelled; do not update status.
        } else {
            definitionStatus = .syntaxError
        }
    }

    private func updateTestResult(index: Int, exitStatus: Int32, output: String, elapsed: TimeInterval) {
        let test = tests[index - 1]
        if exitStatus == 0 {
            if output == "illegal-sexp-in-test/answer" {
                test.status = .syntaxError
                engine?.cancelAllTestsOperation()
            } else if output == "parse-error-in-test/answer" {
                test.status = .parseError
                engine?.cancelAllTestsOperation()
            } else if output == "illegal-sexp-in-defn" || output == "parse-error-in-defn" {
                test.status = .thinking
            } else if output == "()" {
                test.status = .failed(elapsed: elapsed)
                engine?.cancelAllTestsOperation()
            } else {
                test.status = .succeeded(elapsed: elapsed)
            }
        } else if exitStatus == 15 {
            // SIGTERM — operation was cancelled; do not update status.
        } else {
            test.status = .syntaxError
        }
    }

    private func updateAllTestsResult(exitStatus: Int32, output: String, elapsed: TimeInterval) {
        if exitStatus == 0 {
            if output == "fail" {
                bestGuessText = ""
                bestGuessStatus = .failed(elapsed: elapsed)
                engine?.cancelAllOperations()
            } else if output == "illegal-sexp-in-defn" ||
                      output == "parse-error-in-defn" ||
                      output == "illegal-sexp-in-test/answer" ||
                      output == "parse-error-in-test/answer" {
                bestGuessText = ""
                bestGuessStatus = .thinking
            } else {
                bestGuessText = output
                bestGuessStatus = .succeeded(elapsed: elapsed)
                // All tests passed — update definition and individual test statuses directly,
                // since cancelling their operations may prevent them from reporting back.
                definitionStatus = .idle
                for test in tests where test.isActive {
                    if test.status == .thinking {
                        test.status = .succeeded(elapsed: elapsed)
                    }
                }
                engine?.cancelAllOperations()
            }
        } else if exitStatus == 15 {
            bestGuessText = ""
            bestGuessStatus = .idle
        } else {
            bestGuessText = ""
            bestGuessStatus = .idle
        }
    }
}
