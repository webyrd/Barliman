//
//  ContentView.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import SwiftUI

/// Top-level view: definition editor, test rows, and best-guess result pane.
struct ContentView: View {
    @Bindable var model: BarlimanModel

    var body: some View {
        VSplitView {
            // Upper pane: definition editor
            VStack(alignment: .leading, spacing: 4) {
                HStack {
                    Text("Scheme Definition")
                        .font(.system(size: 11, weight: .medium))
                        .foregroundStyle(.secondary)
                    Spacer()
                    statusView(status: model.definitionStatus)
                }
                SchemeTextEditor(
                    text: $model.definitionText,
                    textColor: model.definitionStatus.nsColor,
                    onTextChange: { model.textDidChange() }
                )
            }
            .padding(.horizontal, 8)
            .padding(.top, 8)
            .frame(minHeight: 80)

            // Middle: test rows
            VStack(spacing: 4) {
                ForEach(model.tests) { test in
                    TestRowView(test: test, onTextChange: { model.textDidChange() })
                }
            }
            .padding(.horizontal, 8)
            .padding(.vertical, 4)

            // Lower pane: best guess
            VStack(alignment: .leading, spacing: 4) {
                HStack {
                    Text("Best Guess")
                        .font(.system(size: 11, weight: .medium))
                        .foregroundStyle(.secondary)
                    Spacer()
                    statusView(status: model.bestGuessStatus)
                }
                SchemeTextEditor(
                    text: $model.bestGuessText,
                    isEditable: false,
                    textColor: model.bestGuessStatus.nsColor,
                    onTextChange: nil
                )
            }
            .padding(.horizontal, 8)
            .padding(.bottom, 8)
            .frame(minHeight: 60)
        }
        .frame(minWidth: 600, minHeight: 400)
    }

    @ViewBuilder
    private func statusView(status: TaskStatus) -> some View {
        HStack(spacing: 4) {
            if status.isSpinning {
                ProgressView()
                    .controlSize(.small)
            }
            Text(status.label)
                .font(.system(size: 11))
                .foregroundStyle(status.color)
        }
    }
}
