//
//  ProcessTracker.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Foundation

/// Tracks all launched Chez Scheme processes so they can be killed on app exit or crash.
final class ProcessTracker {
    static let shared = ProcessTracker()

    private var lock = NSLock()
    private var runningPIDs = Set<pid_t>()

    private init() {
        // Register atexit handler as a safety net for abnormal termination
        atexit {
            ProcessTracker.shared.terminateAll()
        }
    }

    func register(_ pid: pid_t) {
        lock.lock()
        runningPIDs.insert(pid)
        lock.unlock()
    }

    func unregister(_ pid: pid_t) {
        lock.lock()
        runningPIDs.remove(pid)
        lock.unlock()
    }

    func terminateAll() {
        lock.lock()
        let pids = runningPIDs
        runningPIDs.removeAll()
        lock.unlock()
        for pid in pids {
            kill(pid, SIGKILL)
        }
    }
}
