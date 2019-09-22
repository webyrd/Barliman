//
//  SemanticsWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/29/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

class SemanticsWindowController: NSWindowController {

    // Making evaluationRulesView a weak reference seems to cause a runtime error.  Why?
    @IBOutlet var evaluationRulesView: NSTextView!
    
    var editorWindowController: EditorWindowController?
    
    
    override var windowNibName: String? {
        return "SemanticsWindowController"
    }

    func textDidChange(_ notification: Notification) {
        // NSTextView text changed
        print("@@@@@@@@@@@@@@@@@@@ semantics textDidChange")
        editorWindowController!.setupRunCodeFromEditPaneTimer()
    }
    
    override func windowDidLoad() {
        super.windowDidLoad()
        
        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
        
        // from http://stackoverflow.com/questions/19801601/nstextview-with-smart-quotes-disabled-still-replaces-quotes
        evaluationRulesView.isAutomaticQuoteSubstitutionEnabled = false
        
        loadInterpreterCode("interp")
    }
    
    func loadInterpreterCode(_ interpFileName: String) {
        // get the path to the application's bundle, so we can load the interpreter file
        let bundle = Bundle.main
        
        let interp_path: NSString? = bundle.path(forResource: interpFileName, ofType: "scm", inDirectory: "mk-and-rel-interp") as NSString?
        
        let path = URL(fileURLWithPath: interp_path! as String)
        
        // from http://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file
        do {
            let text = try NSString(contentsOf: path, encoding: String.Encoding.utf8.rawValue)
            evaluationRulesView.textStorage?.setAttributedString(NSAttributedString(string: text as String))
        }
        catch {
            print("Oh noes!  Can't load interpreter for Semantics Window!")
        }
    }
    
    @IBAction func loadFullMiniSchemeWithMatch(_ sender: NSMenuItem) {
        loadInterpreterCode("interp")
        print("@@@@ loaded FullMiniSchemeWithMatch interpreter from popup menu")
        editorWindowController!.setupRunCodeFromEditPaneTimer()
    }
    
    @IBAction func loadCallByValueLambdaCalculus(_ sender: NSMenuItem) {
        loadInterpreterCode("cbv-lc")
        print("@@@@ loaded CallByValueLambdaCalculus interpreter from popup menu")
        editorWindowController!.setupRunCodeFromEditPaneTimer()
    }

    @IBAction func loadDynamicallyScopedMiniSchemeWithMatch(_ sender: NSMenuItem) {
        loadInterpreterCode("interp-dynamic")
        print("@@@@ loaded DynamicallyScopedMiniSchemeWithMatch interpreter from popup menu")
        editorWindowController!.setupRunCodeFromEditPaneTimer()
    }

    
    func getInterpreterCode() -> String {
        return (evaluationRulesView.textStorage?.string)!
    }
    

    func cleanup() {
        // application is about to quit -- clean up!
    }

}

