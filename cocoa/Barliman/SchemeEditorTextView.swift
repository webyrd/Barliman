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
        super.keyDown(event)
        Swift.print("----------------   keyDown: \(event.keyCode) ")
        
        if event.keyCode == 25 {
            // if a left-paren is entered, enter a right-paren as well, 
            // and move the cursor between the parens
            Swift.print("---------------- left paren")
            self.textStorage?.insertAttributedString(NSAttributedString(string: ")"), atIndex: self.selectedRange.location)
            self.selectedRange.location = self.selectedRange.location - 1
        }
        
    }

}
