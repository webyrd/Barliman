//
//  EditorWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

class EditorWindowController: NSWindowController {

    @IBOutlet weak var schemeDefinitionField: NSTextField!
    
    @IBOutlet weak var test1InputField: NSTextField!
    @IBOutlet weak var test1ExpectedOutputField: NSTextField!

    @IBOutlet weak var test2InputField: NSTextField!
    @IBOutlet weak var test2ExpectedOutputField: NSTextField!

    @IBOutlet weak var test3InputField: NSTextField!
    @IBOutlet weak var test3ExpectedOutputField: NSTextField!

    
    let processingQueue: NSOperationQueue = NSOperationQueue()
    
    override var windowNibName: String? {
        return "EditorWindowController"
    }
    
    override func windowDidLoad() {
        super.windowDidLoad()

        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
    }

    func cleanup() {
        // application is about to quit -- clean up!
        
        print("cleaning up!")
        
        // tell every operation to kill its Scheme task
        print("prior operation count: \(processingQueue.operationCount)")
        processingQueue.cancelAllOperations()
        
        //
        
        // wait until all the operations have finished
        processingQueue.waitUntilAllOperationsAreFinished()
        print("subsequent operation count: \(processingQueue.operationCount)")
        
        if processingQueue.operationCount > 0 {
            // handle this better!  :)
            print("$$$$  Oh noes!  Looks like there is a Scheme process still running!")
        }
    }
    
    override func controlTextDidChange(aNotification: NSNotification) {
        // called whenever the text in editableSchemeField changes
        runCodeFromEditPane()
    }
    
    func runCodeFromEditPane() {
        
        // The text in the code pane changed!  Launch a new Scheme task to evaluate the new expression...
        
        // see how many operations are currently in the queue
        print("operation count: \(processingQueue.operationCount)")
        
        // send a signal to cancel the running Scheme task, to every operation in the processing queue
        //
        // it is the responsibility of the operations to check for the cancel signal
        processingQueue.cancelAllOperations()
        
        // get the path to the application's bundle, so we can load the miniKanren and interpreter files
        // into Chez
        let bundle = NSBundle.mainBundle()
        
        let mk_vicare_path: NSString? = bundle.pathForResource("mk-vicare", ofType: "scm", inDirectory: "mk-and-rel-interp/mk")
        let mk_path: NSString? = bundle.pathForResource("mk", ofType: "scm", inDirectory: "mk-and-rel-interp/mk")
        let interp_path: NSString? = bundle.pathForResource("interp", ofType: "scm", inDirectory: "mk-and-rel-interp")
        
        // write the Scheme code containing the miniKanren query to a temp file
        let query_file_simple = "barliman-query-simple.scm"
        let query_file_test1 = "barliman-query-test1.scm"
        let query_file_test2 = "barliman-query-test2.scm"
        let query_file_test3 = "barliman-query-test3.scm"

        
        let mk_vicare_path_string = mk_vicare_path as! String
        let mk_path_string = mk_path as! String
        let interp_path_string = interp_path as! String

        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"
        let load_interp_string: String = "(load \"\( interp_path_string )\")"

        let querySimple: String = load_mk_vicare_string +
                           load_mk_string +
                           load_interp_string +
            "(write (run 1 (q) (fresh (A B C D E F G Z) (evalo `(begin " + schemeDefinitionField.stringValue + " ,Z) q))) )"
        
        let queryTest1: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            "(write (run 1 (q) (fresh (A B C D E F G) (evalo `(begin " + schemeDefinitionField.stringValue + " " + test1InputField.stringValue + ") " + test1ExpectedOutputField.stringValue + "))) )"

        let queryTest2: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            "(write (run 1 (q) (fresh (A B C D E F G) (evalo `(begin " + schemeDefinitionField.stringValue + " " + test2InputField.stringValue + ") " + test2ExpectedOutputField.stringValue + "))) )"

        let queryTest3: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            "(write (run 1 (q) (fresh (A B C D E F G) (evalo `(begin " + schemeDefinitionField.stringValue + " " + test3InputField.stringValue + ") " + test3ExpectedOutputField.stringValue + "))) )"

        
        print("querySimple = \n\( querySimple )\n")
        print("queryTest1 = \n\( queryTest1 )\n")
        print("queryTest2 = \n\( queryTest2 )\n")
        print("queryTest3 = \n\( queryTest3 )\n")

        print("\ntest1InputField.stringValue = \( test1InputField.stringValue )\n")
        
        
        var pathSimple: NSURL
        pathSimple = NSURL()
        
        var pathTest1: NSURL
        pathTest1 = NSURL()
        
        var pathTest2: NSURL
        pathTest2 = NSURL()
        
        var pathTest3: NSURL
        pathTest3 = NSURL()


        
        // write the temporary file containing the query to the user's Document directory.  This seems a bit naughty.  Where is the right place to put this?  In ~/.barliman, perhaps?
        if let dir = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true).first {
            
            pathSimple = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_simple)
            pathTest1 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test1)
            pathTest2 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test2)
            pathTest3 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test3)
            
            // write the query files
            do {
                try querySimple.writeToURL(pathSimple, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest1.writeToURL(pathTest1, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest2.writeToURL(pathTest2, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest3.writeToURL(pathTest3, atomically: false, encoding: NSUTF8StringEncoding)
            }
            catch {
                // this error handling could be better!  :)
                print("couldn't write to query files")
            }
        }
        
        
        // paths to the Schemes file containing the miniKanren query
        var schemeScriptPathStringSimple: String = ""
        schemeScriptPathStringSimple = pathSimple.path!
        
        var schemeScriptPathStringTest1: String = ""
        schemeScriptPathStringTest1 = pathTest1.path!
        
        var schemeScriptPathStringTest2: String = ""
        schemeScriptPathStringTest2 = pathTest2.path!

        var schemeScriptPathStringTest3: String = ""
        schemeScriptPathStringTest3 = pathTest3.path!

        
        // create the operations that will be placed in the operation queue
        let runSchemeOpSimple: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringSimple, taskType: "simple")
        
        let runSchemeOpTest1: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest1, taskType: "test1")
        
        let runSchemeOpTest2: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest2, taskType: "test2")
        
        let runSchemeOpTest3: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest3, taskType: "test3")

        
        // wait until the previous operations kill their tasks and finish, before adding the new operations
        processingQueue.waitUntilAllOperationsAreFinished()
        
        // now that the previous operations have completed, safe to add the new operations
        processingQueue.addOperation(runSchemeOpSimple)
        if !test1InputField.stringValue.isEmpty && !test1ExpectedOutputField.stringValue.isEmpty {
            print("queuing test1")
            processingQueue.addOperation(runSchemeOpTest1)
        }
        if !test2InputField.stringValue.isEmpty && !test2ExpectedOutputField.stringValue.isEmpty {
            print("queuing test2")
            processingQueue.addOperation(runSchemeOpTest2)
        }
        if !test3InputField.stringValue.isEmpty && !test3ExpectedOutputField.stringValue.isEmpty {
            print("queuing test3")
            processingQueue.addOperation(runSchemeOpTest3)
        }
    }

    
}
