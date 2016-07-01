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
        
        if event.keyCode == 0x31 {
            // space was entered
            Swift.print("---------------- space")

            let cursorPos = self.selectedRange.location
            let myString : String = (self.string)!
            Swift.print("myString: \( myString )")
            let rightIndex = myString.startIndex.advancedBy(cursorPos)
            let leftIndex = (rightIndex == myString.startIndex) ? myString.startIndex : myString.startIndex.advancedBy(cursorPos).predecessor()
            
            let leftChar = (leftIndex == myString.startIndex) ? " " : myString[leftIndex]
            let rightChar = (rightIndex == myString.endIndex) ? " " : myString[rightIndex]
            
            Swift.print("left char: \( leftChar )")
            Swift.print("right char: \( rightChar )")

            if (leftChar == "(" || leftChar == " " || leftChar == "\n") && (rightChar == ")" || rightChar == " " ||  rightChar == "\n") {
                self.textStorage?.insertAttributedString(NSAttributedString(string: getNextUnusedLogicVar(myString)), atIndex: self.selectedRange.location)
                // adapted from http://stackoverflow.com/questions/30093688/how-to-create-range-in-swift
                let range = NSRange(location: cursorPos, length: 2)
                self.setSelectedRange(range)
            } else {
                super.keyDown(event)
                if self.string == " " {
                    self.string = getNextUnusedLogicVar(self.string!)
                    let range = NSRange(location: 0, length: 2)
                    self.setSelectedRange(range)
                }
            }
            
        } else if event.keyCode == 25 {
            // left-paren was entered
            Swift.print("---------------- left paren")
            
            super.keyDown(event)

            self.textStorage?.insertAttributedString(NSAttributedString(string: ")"), atIndex: self.selectedRange.location)
            self.selectedRange.location = self.selectedRange.location - 1
        } else if event.keyCode == 29 {
            // right-paren was entered
            
            Swift.print("---------------- right paren")

            // ignore the paren!  right parens should only come from typing left parens!

        } else if event.keyCode == 51 {
            // delete key was entered

            // just delete normally for now.  Should refuse to delete right parens, instead moving to the left one character
            super.keyDown(event)
            
        } else if event.keyCode == 36 {
            // return key was entered
    
            // just act normally for now.  Should auto-indent on the next line
            super.keyDown(event)

        } else {
            super.keyDown(event)
            
            Swift.print("keyCode: \( event.keyCode )")

        }
    }
    
    
    func getNextUnusedLogicVar(str: String) -> String {
        //adapted from http://stackoverflow.com/questions/24034043/how-do-i-check-if-a-string-contains-another-string-in-swift
        if str.rangeOfString(",A") == nil{
            return ",A"
        }
        if str.rangeOfString(",B") == nil{
            return ",B"
        }
        if str.rangeOfString(",C") == nil{
            return ",C"
        }
        if str.rangeOfString(",D") == nil{
            return ",D"
        }
        if str.rangeOfString(",E") == nil{
            return ",E"
        }
        if str.rangeOfString(",F") == nil{
            return ",F"
        }
        if str.rangeOfString(",G") == nil{
            return ",G"
        }
        if str.rangeOfString(",H") == nil{
            return ",H"
        }
        if str.rangeOfString(",I") == nil{
            return ",I"
        }
        if str.rangeOfString(",J") == nil{
            return ",J"
        }
        if str.rangeOfString(",K") == nil{
            return ",K"
        }
        if str.rangeOfString(",L") == nil{
            return ",L"
        }
        if str.rangeOfString(",M") == nil{
            return ",M"
        }
        if str.rangeOfString(",N") == nil{
            return ",N"
        }
        if str.rangeOfString(",O") == nil{
            return ",O"
        }
        if str.rangeOfString(",P") == nil{
            return ",P"
        }
        if str.rangeOfString(",Q") == nil{
            return ",Q"
        }
        if str.rangeOfString(",R") == nil{
            return ",R"
        }
        if str.rangeOfString(",S") == nil{
            return ",S"
        }
        if str.rangeOfString(",T") == nil{
            return ",T"
        }
        if str.rangeOfString(",U") == nil{
            return ",U"
        }
        if str.rangeOfString(",V") == nil{
            return ",V"
        }
        if str.rangeOfString(",W") == nil{
            return ",W"
        }
        if str.rangeOfString(",X") == nil{
            return ",X"
        }
        if str.rangeOfString(",Y") == nil{
            return ",Y"
        }
        if str.rangeOfString(",Z") == nil{
            return ",Z"
        }
        
        // out of variables!
        return ""
    }
    

}
