//
//  SemanticsWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/29/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

class SemanticsWindowController: NSWindowController {

    // Making this a weak reference seems to cause a runtime error.  Why?
    @IBOutlet var evaluationRulesView: NSTextView!

    override var windowNibName: String? {
        return "SemanticsWindowController"
    }

    override func windowDidLoad() {
        super.windowDidLoad()
        
        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
        
        // from http://stackoverflow.com/questions/19801601/nstextview-with-smart-quotes-disabled-still-replaces-quotes
        evaluationRulesView.automaticQuoteSubstitutionEnabled = false
        
        evaluationRulesView.textStorage?.setAttributedString(NSAttributedString(string: "(foo)"))
        
    }
    

    func cleanup() {
        // application is about to quit -- clean up!
    }

}