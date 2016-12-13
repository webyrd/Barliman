//
//  SchemeEditorTextView.swift
//  Barliman
//
//  Created by William Byrd on 6/11/16.
//  Copyright © 2016 William E. Byrd. All rights reserved.
//

import Cocoa

class SchemeEditorTextView: NSTextView {

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)

        // Drawing code here.
    }
    
    override func keyDown(with event: NSEvent) {
        Swift.print("----------------   keyDown: \(event.keyCode) ")
        
        if ((event.keyCode == 0x31) && (event.modifierFlags.contains(NSEventModifierFlags.control))) {
            // space was entered while holding the 'control' key
            Swift.print("---------------- space + control")

            let oldString : String = (self.string)!
            // Swift.print("undo keydown oldString: \( oldString )")
            
            let unusedLogicVarString : String = getNextUnusedLogicVar(oldString)
            
            let newPartialString : NSMutableAttributedString = NSMutableAttributedString(string: unusedLogicVarString,
                                                                                   attributes: [NSFontAttributeName:NSFont(
                                                                                   name: EditorWindowController.fontName(),
                                                                                   size: EditorWindowController.fontSize())!])
            
            updateTextStorage(newPartialString)
            
        } else {
            super.keyDown(with: event)
            
            Swift.print("keyCode: \( event.keyCode )")
        }
    }
    
    // Adapted from http://nshipster.com/nsundomanager/
    func updateTextStorage(_ newPartialString: NSAttributedString) {
        
        Swift.print("undo updateTextStorage called with string: \( newPartialString.string )")

        let oldAttrString : NSAttributedString = self.attributedString()
        let oldAttrStringCopy : NSMutableAttributedString = NSMutableAttributedString.init(attributedString: oldAttrString)

        let undoState : [String:AnyObject] = ["attrString": oldAttrStringCopy, "selectedRange": self.selectedRange as AnyObject]
        
        let undoManager = self.undoManager
        undoManager!.registerUndo(withTarget: self, selector: #selector(undoTextStorage(_:)), object: undoState)
        
        Swift.print("undo message will contain string: \( oldAttrStringCopy.string )")
        
        self.textStorage?.replaceCharacters(in: self.selectedRange, with: newPartialString)
        
        self.didChangeText()
    }

    // Adapted from http://nshipster.com/nsundomanager/
    @objc func undoTextStorage(_ undoState: [String:AnyObject]) {
        
        let newAttrString : NSAttributedString = undoState["attrString"] as! NSAttributedString
        let selectedRange : NSRange = undoState["selectedRange"] as! NSRange
        
        Swift.print("undo undoTextStorage called with string: \( newAttrString.string )")
        
        
        let oldAttrString : NSAttributedString = self.attributedString()
        let oldAttrStringCopy : NSMutableAttributedString = NSMutableAttributedString.init(attributedString: oldAttrString)
        
        // Adapted from http://stackoverflow.com/questions/24970713/pass-tuples-as-anyobject-in-swift
        let undoState : [String:AnyObject] = ["attrString": oldAttrStringCopy, "selectedRange": self.selectedRange as AnyObject]
        
        let undoManager = self.undoManager
        undoManager!.registerUndo(withTarget: self, selector: #selector(undoTextStorage(_:)), object: undoState)
        
        Swift.print("undo undoTextStorage will contain string: \( oldAttrStringCopy.string )")
        
        self.textStorage?.setAttributedString(newAttrString)
        self.setSelectedRange(selectedRange)
        
        self.didChangeText()
    }

    
    fileprivate
    static let variables = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".characters.map { "," + String($0) }

    func getNextUnusedLogicVar(_ str: String) -> String {
        //adapted from http://stackoverflow.com/questions/24034043/how-do-i-check-if-a-string-contains-another-string-in-swift

        return SchemeEditorTextView.variables.find {
            str.range(of: $0) == nil
        } ?? ""
    }

}
