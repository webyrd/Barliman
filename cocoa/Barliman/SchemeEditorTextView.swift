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
    
    // from http://stackoverflow.com/questions/27880650/swift-extract-regex-matches
    // and http://stackoverflow.com/questions/24094872/cant-pass-0-to-nsregularexpression-options
    func matchesForRegexInText(regex: String!, text: String!) -> [String] {
        
        var re: NSRegularExpression?
        do {
            re = try NSRegularExpression(pattern: regex, options: [])
        } catch {
            Swift.print("invalid regex")
        }
        
        let nsString = text as NSString
        let results = re!.matchesInString(text,
                                            options: [], range: NSMakeRange(0, nsString.length))
        return results.map({ nsString.substringWithRange($0.range)})
    }
    
// For now, don't worry about structured editing, since it is messing up undo.
//
//    override func keyDown(event: NSEvent) {
//        super.keyDown(event)
//        Swift.print("----------------   keyDown: \(event.keyCode) ")
//        
//        if event.keyCode == 25 {
//            // left-paren was entered
//            Swift.print("---------------- left paren")
//            
//            self.textStorage?.insertAttributedString(NSAttributedString(string: ")"), atIndex: self.selectedRange.location)
//            self.selectedRange.location = self.selectedRange.location - 1
//        }
//    }

}
