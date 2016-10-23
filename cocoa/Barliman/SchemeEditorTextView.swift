//
//  SchemeEditorTextView.swift
//  Barliman
//
//  Created by William Byrd on 6/11/16.
//  Copyright © 2016 William E. Byrd. All rights reserved.
//

import Cocoa

class SchemeEditorTextView: NSTextView {

    override func drawRect(dirtyRect: NSRect) {
        super.drawRect(dirtyRect)

        // Drawing code here.
    }
    
    override func keyDown(event: NSEvent) {
        Swift.print("----------------   keyDown: \(event.keyCode) ")
        
        if ((event.keyCode == 0x31) && (event.modifierFlags.contains(NSEventModifierFlags.Control))) {
            // space was entered while holding the 'control' key
            Swift.print("---------------- space + control")

            let myString : String = (self.string)!
            Swift.print("myString: \( myString )")
            
            let str : String = getNextUnusedLogicVar(myString)
            let attrString : NSMutableAttributedString = NSMutableAttributedString(string: str,
                                                                                   attributes: [NSFontAttributeName:NSFont(
                                                                                    name: EditorWindowController.fontName(),
                                                                                    size: EditorWindowController.fontSize())!])
            
            self.textStorage?.insertAttributedString(attrString, atIndex: self.selectedRange.location)
            // adapted from http://stackoverflow.com/questions/30093688/how-to-create-range-in-swift
            
            self.didChangeText()
        } else {
            super.keyDown(event)
            
            Swift.print("keyCode: \( event.keyCode )")
        }
    }

    private
    static let variables = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".characters.map { "," + String($0) }

    func getNextUnusedLogicVar(str: String) -> String {
        //adapted from http://stackoverflow.com/questions/24034043/how-do-i-check-if-a-string-contains-another-string-in-swift

        return SchemeEditorTextView.variables.find {
            str.rangeOfString($0) == nil
        } ?? ""
    }

}
