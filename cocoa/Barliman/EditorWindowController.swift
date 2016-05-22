//
//  EditorWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd. All rights reserved.

import Cocoa

class EditorWindowController: NSWindowController {

    @IBOutlet weak var editableSchemeField: NSTextField!
    @IBOutlet weak var evaluatedEditableSchemeField: NSTextField!
    
    let processingQueue: NSOperationQueue = NSOperationQueue()
    
    override var windowNibName: String? {
        return "EditorWindowController"
    }
    
    override func windowDidLoad() {
        super.windowDidLoad()

        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
    }
    
    
    override func controlTextDidChange(aNotification: NSNotification) {
        // called whenever the text in editableSchemeField changes
        
        // too expensive!  should be run asynchronously, perhaps using GCD
        runCodeFromEditPane()
    }
    
    
    func runCodeFromEditPane() {
        
        print("operation count: \(processingQueue.operationCount)")
        
        // there was a new keystroke!  new code to try to evaluate...
        
        // time it!
        let start : UInt64 = mach_absolute_time()
        
        // send a cancel signal to every operation in the processing queue
        //
        // it is the responsibility of the operations to check for the cancel signal
        processingQueue.cancelAllOperations()
        
        
        let scheme_directory = "/Users/webyrd/github/Barliman/mk-and-rel-interp"
        
        // write Scheme code to the file
        let query_file = "barliman-query.scm" //this is the file. we will write to and read from it
        
        let text = "(load \"\( scheme_directory )/mk/mk-vicare.scm\")" +
            "(load \"\( scheme_directory )/mk/mk.scm\")" +
            "(load \"\( scheme_directory )/interp.scm\")" +
            "(write " + "(run 1 (q) (fresh (A B C D E F G) (evalo `" +
            editableSchemeField.stringValue +
        " q))) )"
        
        var path: NSURL
        path = NSURL()
        
        if let dir = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true).first {
            path = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file)
            
            //writing
            do {
                try text.writeToURL(path, atomically: false, encoding: NSUTF8StringEncoding)
            }
            catch {
                print("couldn't write to query file")
            }
            
        }

        
        // spawn a new operation to run the Scheme task
        processingQueue.addOperationWithBlock() {
            
            let task = NSTask()
            task.launchPath = "/usr/local/bin/scheme"
            
            var myPathString: String
            myPathString = ""
            myPathString = path.path!
            
            task.arguments = ["--script", myPathString]
            
            let pipe = NSPipe()
            let errorPipe = NSPipe()
            task.standardOutput = pipe
            
            task.standardError = errorPipe
            
            
            
            task.launch()
            
            let fileHandle = pipe.fileHandleForReading
            let errorFileHandle = errorPipe.fileHandleForReading
            
            let data = fileHandle.readDataToEndOfFile()
            let errorData = errorFileHandle.readDataToEndOfFile()
            
            
            
            task.waitUntilExit()
            
            let status = task.terminationStatus
            
            
            NSOperationQueue.mainQueue().addOperationWithBlock {
                
                if status == 0 {
                    self.editableSchemeField.textColor = NSColor.blackColor()
                } else {
                    self.editableSchemeField.textColor = NSColor.redColor()
                    self.evaluatedEditableSchemeField.stringValue = ""
                }
                
                
                let datastring = NSString(data: data, encoding: NSUTF8StringEncoding) as! String
                
                if status == 0 {
                    self.evaluatedEditableSchemeField.stringValue = datastring
                }
        
                let errorDatastring = NSString(data: errorData, encoding: NSUTF8StringEncoding) as! String
                print("error datastring: \(errorDatastring)")
            }
            
        }
        
        let duration : UInt64 = mach_absolute_time() - start
        
        var info : mach_timebase_info = mach_timebase_info(numer: 0, denom: 0)
        mach_timebase_info(&info)
        
        let total = (duration * UInt64(info.numer) / UInt64(info.denom)) / 1_000_000
        print("time: \(total) µs.")
    
    }

    
}
