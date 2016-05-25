//
//  RunSchemeOperation.swift
//  Barliman
//
//  Created by William Byrd on 5/24/16.
//  Copyright © 2016 William E. Byrd. 
//  Released under MIT License (see LICENSE file)

import Foundation
import Cocoa

class RunSchemeOperation: NSOperation {

    var editorWindowController: EditorWindowController
    var schemeScriptPathString: String
    var task: NSTask
    
    init(editorWindowController: EditorWindowController, schemeScriptPathString: String) {
        self.editorWindowController = editorWindowController
        self.schemeScriptPathString = schemeScriptPathString
        self.task = NSTask()
    }
    
    override func cancel() {
        print("!!! cancel called!")
        super.cancel()
        print("&&& killing process \( task.processIdentifier )")
        task.terminate()
        print("&&& killed process")
    }
    
    override func main() {
        
        if self.cancelled {
            print("*** cancelled immediately! ***\n")
            return
        }
        
        runSchemeCode()
    }

    func runSchemeCode() {
    
        // Path to Chez Scheme
        // Perhaps this should be settable in a preferences panel.
        task.launchPath = "/usr/local/bin/scheme"
        
        // Arguments to Chez Scheme
        task.arguments = ["--script", self.schemeScriptPathString]
        
        let outputPipe = NSPipe()
        let errorPipe = NSPipe()
        
        task.standardOutput = outputPipe
        task.standardError = errorPipe
        
        // Launch the Chez Scheme process, with the miniKanren query
        task.launch()
        print("*** launched process \( task.processIdentifier )")

        
        let outputFileHandle = outputPipe.fileHandleForReading
        let errorFileHandle = errorPipe.fileHandleForReading
        
        let data = outputFileHandle.readDataToEndOfFile()
        let errorData = errorFileHandle.readDataToEndOfFile()
        
        // wait until the miniKanren query completes
        // (or until the task is killed because the operation is cancelled)
        task.waitUntilExit()
        
        // we need the exit status of the Scheme process to know if Chez choked because of a syntax error (for a malformed query), or whether Chez exited cleanly
        let exitStatus = task.terminationStatus
        
        
        // update the user interface, which *must* be done through the main thread
        NSOperationQueue.mainQueue().addOperationWithBlock {
            
            let datastring = NSString(data: data, encoding: NSUTF8StringEncoding) as! String
            let errorDatastring = NSString(data: errorData, encoding: NSUTF8StringEncoding) as! String

            if exitStatus == 0 {
                self.editorWindowController.editableSchemeField.textColor = NSColor.blackColor()
                self.editorWindowController.evaluatedEditableSchemeField.stringValue = datastring
            } else {
                self.editorWindowController.editableSchemeField.textColor = NSColor.redColor()
                self.editorWindowController.evaluatedEditableSchemeField.stringValue = ""
            }
            
            print("datastring for process \( self.task.processIdentifier ): \(datastring)")
            print("error datastring for process \( self.task.processIdentifier ): \(errorDatastring)")
        }
    }
}