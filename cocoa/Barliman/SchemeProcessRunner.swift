//
//  SchemeProcessRunner.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Foundation

/// Runs a single Chez Scheme process for a given query script.
/// Updates the BarlimanModel with results instead of manipulating UI directly.
class SchemeProcessRunner: Operation, @unchecked Sendable {
    let taskType: TaskType
    let scriptPath: String
    private weak var model: BarlimanModel?
    private weak var engine: SchemeEngine?
    private var task = Process()

    init(model: BarlimanModel, engine: SchemeEngine, scriptPath: String, taskType: TaskType) {
        self.model = model
        self.engine = engine
        self.scriptPath = scriptPath
        self.taskType = taskType
    }

    override func cancel() {
        super.cancel()
        if task.isRunning {
            let pid = task.processIdentifier
            print("&&& killing process \(pid)")
            kill(pid, SIGKILL)
            print("&&& killed process \(pid)")
        }
    }

    override func main() {
        if isCancelled {
            print("*** cancelled immediately! ***\n")
            return
        }
        runSchemeCode()
    }

    private func runSchemeCode() {
        let startTime = Date()

        // Resolve Chez Scheme path
        guard let chezPath = ChezSchemeFinder.resolvedPath() else {
            print("*** Chez Scheme not found. Set path with: defaults write com.WilliamEByrd.Barliman ChezSchemePath /path/to/chez")
            return
        }
        task.executableURL = URL(fileURLWithPath: chezPath)
        task.arguments = ["--script", scriptPath]

        let outputPipe = Pipe()
        let errorPipe = Pipe()
        task.standardOutput = outputPipe
        task.standardError = errorPipe

        print("*** launching Scheme process: \(scriptPath)")

        do {
            try task.run()
        } catch {
            print("*** Failed to launch Scheme process: \(error)")
            return
        }

        let pid = task.processIdentifier
        ProcessTracker.shared.register(pid)
        print("*** launched process \(pid)")

        let data = outputPipe.fileHandleForReading.readDataToEndOfFile()
        let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()

        task.waitUntilExit()
        ProcessTracker.shared.unregister(pid)

        let exitStatus = task.terminationStatus
        let elapsed = Date().timeIntervalSince(startTime)
        let output = (String(data: data, encoding: .utf8) ?? "").trimmingCharacters(in: .whitespacesAndNewlines)
        let errorOutput = String(data: errorData, encoding: .utf8) ?? ""

        print("datastring for process \(pid): \(output)")
        if !errorOutput.isEmpty {
            print("error datastring for process \(pid): \(errorOutput)")
        }

        // Update model on main thread
        // Capture taskType and model strongly — the operation may be deallocated
        // by the queue before this block runs.
        let taskType = self.taskType
        guard let model = self.model else { return }
        DispatchQueue.main.async {
            model.updateResult(for: taskType, exitStatus: exitStatus, output: output, elapsed: elapsed)
        }
    }
}
