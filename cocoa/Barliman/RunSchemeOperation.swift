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
    
        task.launchPath = "/usr/local/bin/scheme"
        
        task.arguments = ["--script", self.schemeScriptPathString]
        
        let outputPipe = NSPipe()
        let errorPipe = NSPipe()
        
        task.standardOutput = outputPipe
        task.standardError = errorPipe
        
        
        var schemeTerminated = false
        
        // adapted from https://www.raywenderlich.com/125071/nstask-tutorial-os-x
        task.terminationHandler = {
            task in
            print("*** terminationHander called for process \( task.processIdentifier )")
            schemeTerminated = true
        }
        
        task.launch()
        print("*** launched process \( task.processIdentifier )")

        
        let outputFileHandle = outputPipe.fileHandleForReading
        let errorFileHandle = errorPipe.fileHandleForReading
        
        let data = outputFileHandle.readDataToEndOfFile()
        let errorData = errorFileHandle.readDataToEndOfFile()
        
        var loopCount = 0
        
        print("*** entering loop for process \( task.processIdentifier )")
        while true {
            if cancelled == true {
                print("*** cancelled process \( task.processIdentifier )")
                print("*** loopCount \( loopCount ) for process \( task.processIdentifier )")
                task.terminate()
                return
            } else if schemeTerminated == true {
                print("??? schemeTerminated")
                break
            } else {
                loopCount += 1
            }
        }
        
        print("*** out of loop for process \( task.processIdentifier );  schemeTerminated = \( schemeTerminated )")
        
        
        let status = task.terminationStatus
        
        NSOperationQueue.mainQueue().addOperationWithBlock {
            
            if status == 0 {
                self.editorWindowController.editableSchemeField.textColor = NSColor.blackColor()
            } else {
                self.editorWindowController.editableSchemeField.textColor = NSColor.redColor()
                self.editorWindowController.evaluatedEditableSchemeField.stringValue = ""
            }
            
            
            let datastring = NSString(data: data, encoding: NSUTF8StringEncoding) as! String
            
            if status == 0 {
                self.editorWindowController.evaluatedEditableSchemeField.stringValue = datastring
            }
            
            let errorDatastring = NSString(data: errorData, encoding: NSUTF8StringEncoding) as! String
            print("error datastring for process \( self.task.processIdentifier ): \(errorDatastring)")
        }
    }
}