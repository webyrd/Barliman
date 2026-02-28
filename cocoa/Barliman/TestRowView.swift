//
//  TestRowView.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import SwiftUI

/// A single test row: input field, expected output field, spinner, and status label.
struct TestRowView: View {
    @Bindable var test: TestState
    var onTextChange: () -> Void

    var body: some View {
        HStack(spacing: 8) {
            Text("Test \(test.id)")
                .font(.system(size: 11))
                .foregroundStyle(.secondary)
                .frame(width: 42, alignment: .trailing)

            TextField("Input", text: $test.input)
                .font(.custom("Monaco", size: 14))
                .foregroundStyle(test.status.color)
                .textFieldStyle(.roundedBorder)
                .onChange(of: test.input) { onTextChange() }

            TextField("Expected Output", text: $test.expectedOutput)
                .font(.custom("Monaco", size: 14))
                .foregroundStyle(test.status.color)
                .textFieldStyle(.roundedBorder)
                .onChange(of: test.expectedOutput) { onTextChange() }

            if test.status.isSpinning {
                ProgressView()
                    .controlSize(.small)
                    .frame(width: 16, height: 16)
            } else {
                Spacer()
                    .frame(width: 16, height: 16)
            }

            Text(test.status.label)
                .font(.system(size: 11))
                .foregroundStyle(test.status.color)
                .frame(width: 140, alignment: .leading)
                .lineLimit(1)
        }
    }
}
