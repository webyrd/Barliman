//
//  BarlimanApp.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import SwiftUI

@main
struct BarlimanApp: App {
    @NSApplicationDelegateAdaptor(AppLifecycleDelegate.self) var appDelegate
    @State private var model = BarlimanModel()

    var body: some Scene {
        WindowGroup {
            ContentView(model: model)
                .onAppear {
                    let engine = SchemeEngine(model: model)
                    model.engine = engine
                    appDelegate.engine = engine
                }
        }
    }
}

/// Handles signal handlers and cleanup on app termination.
class AppLifecycleDelegate: NSObject, NSApplicationDelegate {
    var engine: SchemeEngine?

    func applicationDidFinishLaunching(_ notification: Notification) {
        // Install signal handlers to clean up child processes on unexpected termination
        let handler: @convention(c) (Int32) -> Void = { _ in
            ProcessTracker.shared.terminateAll()
            signal(SIGTERM, SIG_DFL)
            signal(SIGINT, SIG_DFL)
            signal(SIGHUP, SIG_DFL)
            raise(SIGTERM)
        }
        signal(SIGTERM, handler)
        signal(SIGINT, handler)
        signal(SIGHUP, handler)
    }

    func applicationShouldTerminate(_ sender: NSApplication) -> NSApplication.TerminateReply {
        // Kill all child processes immediately before termination proceeds
        ProcessTracker.shared.terminateAll()
        engine?.cleanup()
        return .terminateNow
    }

    func applicationWillTerminate(_ notification: Notification) {
        // Belt-and-suspenders: kill again in case new processes were spawned
        ProcessTracker.shared.terminateAll()
    }

    func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
        true
    }
}
