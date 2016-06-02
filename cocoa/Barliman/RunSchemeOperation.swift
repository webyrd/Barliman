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
            
            func onTestCompletion(inputField: NSTextField, outputField: NSTextField, datastring: String) {
                if datastring == "()" {
                    inputField.textColor = NSColor.redColor()
                    outputField.textColor = NSColor.redColor()
                    // TODO - would be polite to cancel the allTests operation as well, since it cannot possibly succeed
                } else {
                    inputField.textColor = NSColor.blackColor()
                    outputField.textColor = NSColor.blackColor()
                }
            }
            
            func onTestSyntaxError(inputField: NSTextField, outputField: NSTextField) {
                inputField.textColor = NSColor.greenColor()
                outputField.textColor = NSColor.greenColor()
            }

            let datastring = NSString(data: data, encoding: NSUTF8StringEncoding) as! String
            let errorDatastring = NSString(data: errorData, encoding: NSUTF8StringEncoding) as! String
            let taskType = self.taskType

            let ewc = self.editorWindowController
            
            if exitStatus == 0 {
                // at least Chez ran to completion!  The query could still have failed, of course
                if self.taskType == "simple" {
                    if datastring == "()" {
                        print("--- turning simple red")
                        ewc.schemeDefinitionView.textColor = NSColor.redColor()
                    } else {
                        print("--- turning simple black")
                        ewc.schemeDefinitionView.textColor = NSColor.blackColor()
                    }
                }
                if taskType == "test1" {
                    onTestCompletion(ewc.test1InputField, outputField: ewc.test1ExpectedOutputField, datastring: datastring)
                }
                if taskType == "test2" {
                    onTestCompletion(ewc.test2InputField, outputField: ewc.test2ExpectedOutputField, datastring: datastring)
                }
                if taskType == "test3" {
                    onTestCompletion(ewc.test3InputField, outputField: ewc.test3ExpectedOutputField, datastring: datastring)
                }
                if taskType == "test4" {
                    onTestCompletion(ewc.test4InputField, outputField: ewc.test4ExpectedOutputField, datastring: datastring)
                }
                if taskType == "test5" {
                    onTestCompletion(ewc.test5InputField, outputField: ewc.test5ExpectedOutputField, datastring: datastring)
                }
                if taskType == "test6" {
                    onTestCompletion(ewc.test6InputField, outputField: ewc.test6ExpectedOutputField, datastring: datastring)
                }
                if self.taskType == "allTests" {
                    if datastring == "fail" {
                        ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: "incompatible" as String))
                    } else {
                        ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: datastring))
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
                    ewc.schemeDefinitionView.textColor = NSColor.greenColor()
                }
                
                if taskType == "test1" {
                    onTestSyntaxError(ewc.test1InputField, outputField: ewc.test1ExpectedOutputField)
                }
                if taskType == "test2" {
                    onTestSyntaxError(ewc.test2InputField, outputField: ewc.test2ExpectedOutputField)
                }
                if taskType == "test3" {
                    onTestSyntaxError(ewc.test3InputField, outputField: ewc.test3ExpectedOutputField)
                }
                if taskType == "test4" {
                    onTestSyntaxError(ewc.test4InputField, outputField: ewc.test4ExpectedOutputField)
                }
                if taskType == "test5" {
                    onTestSyntaxError(ewc.test5InputField, outputField: ewc.test5ExpectedOutputField)
                }
                if taskType == "test6" {
                    onTestSyntaxError(ewc.test6InputField, outputField: ewc.test6ExpectedOutputField)
                }
                if taskType == "allTests" {
                    ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: "syntax error" as String))
                }
            }
            
            print("datastring for process \( self.task.processIdentifier ): \(datastring)")
            print("error datastring for process \( self.task.processIdentifier ): \(errorDatastring)")
        }
    }
}