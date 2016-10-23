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
        
        if ((event.keyCode == 0x31) && (event.modifierFlags.contains(NSEventModifierFlags.Option))) {
            // space was entered while holding the 'option' key
            Swift.print("---------------- space + option")

            let cursorPos = self.selectedRange.location
            let myString : String = (self.string)!
            Swift.print("myString: \( myString )")
            let rightIndex = myString.startIndex.advancedBy(cursorPos)
            let leftIndex = (rightIndex == myString.startIndex) ? myString.startIndex : myString.startIndex.advancedBy(cursorPos).predecessor()
            
            let leftChar = (leftIndex == myString.startIndex) ? " " : myString[leftIndex]
            let rightChar = (rightIndex == myString.endIndex) ? " " : myString[rightIndex]
            
            Swift.print("left char: \( leftChar )")
            Swift.print("right char: \( rightChar )")

            self.textStorage?.insertAttributedString(NSAttributedString(string: getNextUnusedLogicVar(myString)), atIndex: self.selectedRange.location)
            // adapted from http://stackoverflow.com/questions/30093688/how-to-create-range-in-swift
            // let range = NSRange(location: cursorPos, length: 2)
            // self.setSelectedRange(range)
            
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
