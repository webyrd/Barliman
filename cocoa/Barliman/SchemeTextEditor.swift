//
//  SchemeTextEditor.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import SwiftUI
import AppKit

/// NSViewRepresentable wrapper for an NSTextView that supports:
/// - Ctrl+Space to insert the next unused logic variable (,A through ,Z)
/// - Monospace font (Monaco 14pt)
/// - Undo/redo
/// - Automatic quote substitution disabled
struct SchemeTextEditor: NSViewRepresentable {
    @Binding var text: String
    var isEditable: Bool = true
    var textColor: NSColor = .textColor
    var onTextChange: (() -> Void)?

    static let fontName = "Monaco"
    static let fontSize: CGFloat = 14

    func makeCoordinator() -> Coordinator {
        Coordinator(self)
    }

    func makeNSView(context: Context) -> NSScrollView {
        let scrollView = NSScrollView()
        scrollView.hasVerticalScroller = true
        scrollView.hasHorizontalScroller = false
        scrollView.autohidesScrollers = true
        scrollView.borderType = .bezelBorder

        let textView = SchemeNSTextView()
        textView.isEditable = isEditable
        textView.isSelectable = true
        textView.allowsUndo = true
        textView.isRichText = false
        textView.isAutomaticQuoteSubstitutionEnabled = false
        textView.isAutomaticDashSubstitutionEnabled = false
        textView.isAutomaticTextReplacementEnabled = false
        textView.usesFindPanel = true
        textView.font = NSFont(name: Self.fontName, size: Self.fontSize)
            ?? NSFont.userFixedPitchFont(ofSize: Self.fontSize)
        textView.textColor = textColor

        // Layout
        textView.autoresizingMask = [.width]
        textView.isVerticallyResizable = true
        textView.isHorizontallyResizable = false
        textView.textContainer?.widthTracksTextView = true
        textView.textContainer?.containerSize = NSSize(width: 0, height: CGFloat.greatestFiniteMagnitude)

        textView.delegate = context.coordinator
        textView.string = text

        scrollView.documentView = textView

        return scrollView
    }

    func updateNSView(_ scrollView: NSScrollView, context: Context) {
        guard let textView = scrollView.documentView as? SchemeNSTextView else { return }

        // Avoid re-entrancy: only update if the text actually differs
        if textView.string != text {
            textView.string = text
        }
        textView.textColor = textColor
        textView.isEditable = isEditable
    }

    // MARK: - Coordinator

    class Coordinator: NSObject, NSTextViewDelegate {
        var parent: SchemeTextEditor
        private var isUpdating = false

        init(_ parent: SchemeTextEditor) {
            self.parent = parent
        }

        func textDidChange(_ notification: Notification) {
            guard !isUpdating, let textView = notification.object as? NSTextView else { return }
            isUpdating = true
            parent.text = textView.string
            parent.onTextChange?()
            isUpdating = false
        }
    }
}

// MARK: - SchemeNSTextView

/// NSTextView subclass with Ctrl+Space logic variable insertion and undo support.
class SchemeNSTextView: NSTextView {

    private static let variables = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".map { "," + String($0) }

    override func keyDown(with event: NSEvent) {
        if event.keyCode == 0x31 && event.modifierFlags.contains(.control) {
            // Ctrl+Space: insert next unused logic variable
            let currentText = self.string
            let unusedVar = getNextUnusedLogicVar(currentText)
            if !unusedVar.isEmpty {
                let font = NSFont(name: SchemeTextEditor.fontName, size: SchemeTextEditor.fontSize)
                    ?? NSFont.userFixedPitchFont(ofSize: SchemeTextEditor.fontSize)!
                let attrString = NSMutableAttributedString(string: unusedVar,
                                                           attributes: [.font: font])
                insertWithUndo(attrString)
            }
        } else {
            super.keyDown(with: event)
        }
    }

    private func getNextUnusedLogicVar(_ str: String) -> String {
        SchemeNSTextView.variables.first(where: { str.range(of: $0) == nil }) ?? ""
    }

    private func insertWithUndo(_ newPartialString: NSAttributedString) {
        let oldAttrString = NSMutableAttributedString(attributedString: self.attributedString())
        let undoState: [String: AnyObject] = [
            "attrString": oldAttrString,
            "selectedRange": self.selectedRange as AnyObject
        ]
        self.undoManager?.registerUndo(withTarget: self, selector: #selector(undoTextStorage(_:)), object: undoState)
        self.textStorage?.replaceCharacters(in: self.selectedRange, with: newPartialString)
        self.didChangeText()
    }

    @objc func undoTextStorage(_ undoState: [String: AnyObject]) {
        let newAttrString = undoState["attrString"] as! NSAttributedString
        let selectedRange = undoState["selectedRange"] as! NSRange

        let oldAttrString = NSMutableAttributedString(attributedString: self.attributedString())
        let redoState: [String: AnyObject] = [
            "attrString": oldAttrString,
            "selectedRange": self.selectedRange as AnyObject
        ]
        self.undoManager?.registerUndo(withTarget: self, selector: #selector(undoTextStorage(_:)), object: redoState)
        self.textStorage?.setAttributedString(newAttrString)
        self.setSelectedRange(selectedRange)
        self.didChangeText()
    }
}
