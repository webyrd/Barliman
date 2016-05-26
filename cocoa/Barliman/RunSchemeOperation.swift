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
    var taskType: String
    
    init(editorWindowController: EditorWindowController, schemeScriptPathString: String, taskType: String) {
        self.editorWindowController = editorWindowController
        self.schemeScriptPathString = schemeScriptPathString
        self.task = NSTask()
        self.taskType = taskType
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
                // at least Chez ran to completion!  The query could still have failed, of course
                if self.taskType == "simple" {
                    if datastring == "()" {
                        print("--- turning simple red")
                        self.editorWindowController.schemeDefinitionField.textColor = NSColor.redColor()
                    } else {
                        print("--- turning simple black")
                        self.editorWindowController.schemeDefinitionField.textColor = NSColor.blackColor()
                    }
                }
                if self.taskType == "test1" {
                    if datastring == "()" {
                        self.editorWindowController.test1InputField.textColor = NSColor.redColor()
                        self.editorWindowController.test1ExpectedOutputField.textColor = NSColor.redColor()
                    } else {
                        self.editorWindowController.test1InputField.textColor = NSColor.blackColor()
                        self.editorWindowController.test1ExpectedOutputField.textColor = NSColor.blackColor()
                    }
                }
                if self.taskType == "test2" {
                    if datastring == "()" {
                        self.editorWindowController.test2InputField.textColor = NSColor.redColor()
                        self.editorWindowController.test2ExpectedOutputField.textColor = NSColor.redColor()
                    } else {
                        self.editorWindowController.test2InputField.textColor = NSColor.blackColor()
                        self.editorWindowController.test2ExpectedOutputField.textColor = NSColor.blackColor()
                    }
                }
                if self.taskType == "test3" {
                    if datastring == "()" {
                        self.editorWindowController.test3InputField.textColor = NSColor.redColor()
                        self.editorWindowController.test3ExpectedOutputField.textColor = NSColor.redColor()
                    } else {
                        self.editorWindowController.test3InputField.textColor = NSColor.blackColor()
                        self.editorWindowController.test3ExpectedOutputField.textColor = NSColor.blackColor()
                    }
                }
            } else if exitStatus == 15 {
                // SIGTERM exitStatus -- ignore
                print("SIGTERM !!!")
            } else {
                // the query wasn't even a legal s-expression, according to Chez!
                if self.taskType == "simple" {
                    print("--- turning simple green")
                    print("exitStatus = \( exitStatus )")
                    self.editorWindowController.schemeDefinitionField.textColor = NSColor.greenColor()
                }
                if self.taskType == "test1" {
                    self.editorWindowController.test1InputField.textColor = NSColor.greenColor()
                    self.editorWindowController.test1ExpectedOutputField.textColor = NSColor.greenColor()
                }
                if self.taskType == "test2" {
                    self.editorWindowController.test2InputField.textColor = NSColor.greenColor()
                    self.editorWindowController.test2ExpectedOutputField.textColor = NSColor.greenColor()
                }
                if self.taskType == "test3" {
                    self.editorWindowController.test3InputField.textColor = NSColor.greenColor()
                    self.editorWindowController.test3ExpectedOutputField.textColor = NSColor.greenColor()
                }
            }
            
            print("datastring for process \( self.task.processIdentifier ): \(datastring)")
            print("error datastring for process \( self.task.processIdentifier ): \(errorDatastring)")
        }
    }
}