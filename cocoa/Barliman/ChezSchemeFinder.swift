//
//  ChezSchemeFinder.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Foundation

/// Resolves the path to the Chez Scheme executable.
/// Checks UserDefaults first ("ChezSchemePath"), then searches common install locations.
/// Set a custom path from Terminal:
///   defaults write com.WilliamEByrd.Barliman ChezSchemePath /path/to/chez
enum ChezSchemeFinder {
    private static let searchPaths = [
        "/opt/homebrew/bin/chez",       // Homebrew, Apple Silicon
        "/usr/local/bin/chez",          // Homebrew, Intel
        "/opt/homebrew/bin/scheme",     // Alternate name, Apple Silicon
        "/usr/local/bin/scheme",        // Alternate name, Intel
    ]

    static func resolvedPath() -> String? {
        // 1. Check UserDefaults override
        if let userPath = UserDefaults.standard.string(forKey: "ChezSchemePath"),
           FileManager.default.isExecutableFile(atPath: userPath) {
            return userPath
        }
        // 2. Search known locations
        for path in searchPaths {
            if FileManager.default.isExecutableFile(atPath: path) {
                return path
            }
        }
        return nil
    }
}
