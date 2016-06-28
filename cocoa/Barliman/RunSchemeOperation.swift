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
    
    let kIllegalSexprString = "Illegal sexpression"
    let kSyntaxErrorString = "Syntax error"
    let kEvaluationFailedString = "Evaluation failed"
    
    init(editorWindowController: EditorWindowController, schemeScriptPathString: String, taskType: String) {
        self.editorWindowController = editorWindowController
        self.schemeScriptPathString = schemeScriptPathString
        self.task = NSTask()
        self.taskType = taskType
    }
    
    override func cancel() {
        print("!!! cancel called!")

        super.cancel()
        
        // print("&&& killing process \( task.processIdentifier )")
        task.terminate()
        // print("&&& killed process")
        
    }
    
    func startSpinner() {
        
        // update the user interface, which *must* be done through the main thread
        NSOperationQueue.mainQueue().addOperationWithBlock {
            
            let ewc = self.editorWindowController
            
            switch self.taskType {
            case "simple":   ewc.schemeDefinitionSpinner.startAnimation(self)
            case "allTests": ewc.bestGuessSpinner.startAnimation(self)
            case "test1":    ewc.test1Spinner.startAnimation(self)
            case "test2":    ewc.test2Spinner.startAnimation(self)
            case "test3":    ewc.test3Spinner.startAnimation(self)
            case "test4":    ewc.test4Spinner.startAnimation(self)
            case "test5":    ewc.test5Spinner.startAnimation(self)
            case "test6":    ewc.test6Spinner.startAnimation(self)
            default: print("!!!!!!!!!! SWITCHERROR in startSpinner: unknown taskType: \( self.taskType )\n")
            }
        }
    }
    
    func stopSpinner() {
        
        // update the user interface, which *must* be done through the main thread
        NSOperationQueue.mainQueue().addOperationWithBlock {

            let ewc = self.editorWindowController

            switch self.taskType {
                case "simple":   ewc.schemeDefinitionSpinner.stopAnimation(self)
                case "allTests": ewc.bestGuessSpinner.stopAnimation(self)
                case "test1":    ewc.test1Spinner.stopAnimation(self)
                case "test2":    ewc.test2Spinner.stopAnimation(self)
                case "test3":    ewc.test3Spinner.stopAnimation(self)
                case "test4":    ewc.test4Spinner.stopAnimation(self)
                case "test5":    ewc.test5Spinner.stopAnimation(self)
                case "test6":    ewc.test6Spinner.stopAnimation(self)
                default: print("!!!!!!!!!! SWITCHERROR in stopSpinner: unknown taskType: \( self.taskType )\n")
            }
        }
    }
    
    override func main() {
        
        if self.cancelled {
            // print("*** cancelled immediately! ***\n")
            return
        }
        
        runSchemeCode()
    }

    
    func runSchemeCode() {
        
        startSpinner()
    
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
        // print("*** launched process \( task.processIdentifier )")

        
        let outputFileHandle = outputPipe.fileHandleForReading
        let errorFileHandle = errorPipe.fileHandleForReading
        
        let data = outputFileHandle.readDataToEndOfFile()
        let errorData = errorFileHandle.readDataToEndOfFile()
        
        // wait until the miniKanren query completes
        // (or until the task is killed because the operation is cancelled)
        task.waitUntilExit()
        
        // we need the exit status of the Scheme process to know if Chez choked because of a syntax error (for a malformed query), or whether Chez exited cleanly
        let exitStatus = task.terminationStatus
        
        stopSpinner()
        
        // update the user interface, which *must* be done through the main thread
        NSOperationQueue.mainQueue().addOperationWithBlock {
            
            func onTestCompletion(inputField: NSTextField, outputField: NSTextField, spinner: NSProgressIndicator, label: NSTextField, datastring: String) {

                if datastring == "parse-error" { // failed to parse!
                    inputField.textColor = NSColor.magentaColor()
                    outputField.textColor = NSColor.magentaColor()
                    label.stringValue = self.kSyntaxErrorString
                    
                    // Be polite and cancel the allTests operation as well, since it cannot possibly succeed
                    self.editorWindowController.schemeOperationAllTests?.cancel()
                } else if datastring == "()" { // parsed, but evaluator query failed!
                    inputField.textColor = NSColor.redColor()
                    outputField.textColor = NSColor.redColor()
                    label.stringValue = self.kEvaluationFailedString
                    
                    // Be polite and cancel the allTests operation as well, since it cannot possibly succeed
                    self.editorWindowController.schemeOperationAllTests?.cancel()
                } else { // parsed, and evaluator query succeeded!
                    onTestSuccess(inputField, outputField: outputField, label: label)
                }
            }
            
            func onTestSuccess(inputField: NSTextField, outputField: NSTextField, label: NSTextField) {
                inputField.textColor = NSColor.blackColor()
                outputField.textColor = NSColor.blackColor()
                label.stringValue = ""
            }
            
            func onTestSyntaxError(inputField: NSTextField, outputField: NSTextField, spinner: NSProgressIndicator, label: NSTextField) {
                inputField.textColor = NSColor.greenColor()
                outputField.textColor = NSColor.greenColor()
                label.stringValue = self.kIllegalSexprString
            }

            let datastring = NSString(data: data, encoding: NSUTF8StringEncoding) as! String
            let errorDatastring = NSString(data: errorData, encoding: NSUTF8StringEncoding) as! String
            let taskType = self.taskType

            let ewc = self.editorWindowController
            
            if exitStatus == 0 {
                // at least Chez ran to completion!  The query could still have failed, of course
                if self.taskType == "simple" {
                    if datastring == "parse-error" {
                        ewc.schemeDefinitionView.textColor = NSColor.magentaColor()
                        ewc.definitionStatusLabel.stringValue = self.kSyntaxErrorString
                        
                        // Be polite and cancel the allTests operation as well, since it cannot possibly succeed
                        self.editorWindowController.schemeOperationAllTests?.cancel()
                    } else if datastring == "()" {
                        // print("--- turning simple red")
                        ewc.schemeDefinitionView.textColor = NSColor.redColor()
                        ewc.definitionStatusLabel.stringValue = self.kEvaluationFailedString
                        
                        // Be polite and cancel the allTests operation as well, since it cannot possibly succeed
                        self.editorWindowController.schemeOperationAllTests?.cancel()
                    } else {
                        // print("--- turning simple black")
                        ewc.schemeDefinitionView.textColor = NSColor.blackColor()
                        ewc.definitionStatusLabel.stringValue = ""
                    }
                }
                if taskType == "test1" {
                    onTestCompletion(ewc.test1InputField, outputField: ewc.test1ExpectedOutputField, spinner: ewc.test1Spinner, label: ewc.test1StatusLabel, datastring: datastring)
                }
                if taskType == "test2" {
                    onTestCompletion(ewc.test2InputField, outputField: ewc.test2ExpectedOutputField, spinner: ewc.test2Spinner, label: ewc.test2StatusLabel,datastring: datastring)
                }
                if taskType == "test3" {
                    onTestCompletion(ewc.test3InputField, outputField: ewc.test3ExpectedOutputField, spinner: ewc.test3Spinner, label: ewc.test3StatusLabel,datastring: datastring)
                }
                if taskType == "test4" {
                    onTestCompletion(ewc.test4InputField, outputField: ewc.test4ExpectedOutputField, spinner: ewc.test4Spinner, label: ewc.test4StatusLabel,datastring: datastring)
                }
                if taskType == "test5" {
                    onTestCompletion(ewc.test5InputField, outputField: ewc.test5ExpectedOutputField, spinner: ewc.test5Spinner, label: ewc.test5StatusLabel,datastring: datastring)
                }
                if taskType == "test6" {
                    onTestCompletion(ewc.test6InputField, outputField: ewc.test6ExpectedOutputField, spinner: ewc.test6Spinner, label: ewc.test6StatusLabel,datastring: datastring)
                }
                if self.taskType == "allTests" {
                    if datastring == "fail" {
                        ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: "" as String))
                    } else {
                        ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: datastring))
                        
                        // Be polite and cancel all the other tests, since they must succeed!
                        ewc.processingQueue.cancelAllOperations()
                    }
                }
            } else if exitStatus == 15 {
                // SIGTERM exitStatus
                print("SIGTERM !!!  taskType = \( self.taskType )")
                
                // allTests must have been cancelled by a failing test, meaning there is no way for allTests to succeed
                if self.taskType == "allTests" {
                    ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: "" as String))
                }
                
                // individual tests must have been cancelled by allTests succeeding, meaning that all the individual tests should succeed
                if taskType == "test1" {
                    onTestSuccess(ewc.test1InputField, outputField: ewc.test1ExpectedOutputField, label: ewc.test1StatusLabel)
                }
                if taskType == "test2" {
                    onTestSuccess(ewc.test2InputField, outputField: ewc.test2ExpectedOutputField, label: ewc.test2StatusLabel)
                }
                if taskType == "test3" {
                    onTestSuccess(ewc.test3InputField, outputField: ewc.test3ExpectedOutputField, label: ewc.test3StatusLabel)
                }
                if taskType == "test4" {
                    onTestSuccess(ewc.test4InputField, outputField: ewc.test4ExpectedOutputField, label: ewc.test4StatusLabel)
                }
                if taskType == "test5" {
                    onTestSuccess(ewc.test5InputField, outputField: ewc.test5ExpectedOutputField, label: ewc.test5StatusLabel)
                }
                if taskType == "test6" {
                    onTestSuccess(ewc.test6InputField, outputField: ewc.test6ExpectedOutputField, label: ewc.test6StatusLabel)
                }

                
            } else {
                // the query wasn't even a legal s-expression, according to Chez!
                if self.taskType == "simple" {
                    // print("--- turning simple green")
                    // print("exitStatus = \( exitStatus )")
                    ewc.schemeDefinitionView.textColor = NSColor.greenColor()
                    ewc.definitionStatusLabel.stringValue = self.kIllegalSexprString
                }
                
                if taskType == "test1" {
                    onTestSyntaxError(ewc.test1InputField, outputField: ewc.test1ExpectedOutputField, spinner: ewc.test1Spinner, label: ewc.test1StatusLabel)
                }
                if taskType == "test2" {
                    onTestSyntaxError(ewc.test2InputField, outputField: ewc.test2ExpectedOutputField, spinner: ewc.test2Spinner, label: ewc.test2StatusLabel)
                }
                if taskType == "test3" {
                    onTestSyntaxError(ewc.test3InputField, outputField: ewc.test3ExpectedOutputField, spinner: ewc.test3Spinner, label: ewc.test3StatusLabel)
                }
                if taskType == "test4" {
                    onTestSyntaxError(ewc.test4InputField, outputField: ewc.test4ExpectedOutputField, spinner: ewc.test4Spinner, label: ewc.test4StatusLabel)
                }
                if taskType == "test5" {
                    onTestSyntaxError(ewc.test5InputField, outputField: ewc.test5ExpectedOutputField, spinner: ewc.test5Spinner, label: ewc.test5StatusLabel)
                }
                if taskType == "test6" {
                    onTestSyntaxError(ewc.test6InputField, outputField: ewc.test6ExpectedOutputField, spinner: ewc.test6Spinner, label: ewc.test6StatusLabel)
                }
                if taskType == "allTests" {
                    ewc.bestGuessView.setTextColor(NSColor.blackColor(), range: NSMakeRange(0, (ewc.bestGuessView.textStorage?.length)!))
                    ewc.bestGuessView.textStorage?.setAttributedString(NSAttributedString(string: "" as String))
                }
            }
            
            print("datastring for process \( self.task.processIdentifier ): \(datastring)")
            print("error datastring for process \( self.task.processIdentifier ): \(errorDatastring)")
        }
    }
}