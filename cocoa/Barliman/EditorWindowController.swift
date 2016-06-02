//
//  EditorWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

class EditorWindowController: NSWindowController {

    // Making this a weak reference seems to cause a runtime error.  Why?
    @IBOutlet var schemeDefinitionView: NSTextView!
    @IBOutlet var bestGuessView: NSTextView!

    @IBOutlet weak var test1InputField: NSTextField!
    @IBOutlet weak var test1ExpectedOutputField: NSTextField!

    @IBOutlet weak var test2InputField: NSTextField!
    @IBOutlet weak var test2ExpectedOutputField: NSTextField!

    @IBOutlet weak var test3InputField: NSTextField!
    @IBOutlet weak var test3ExpectedOutputField: NSTextField!

    @IBOutlet weak var test4InputField: NSTextField!
    @IBOutlet weak var test4ExpectedOutputField: NSTextField!

    @IBOutlet weak var test5InputField: NSTextField!
    @IBOutlet weak var test5ExpectedOutputField: NSTextField!

    @IBOutlet weak var test6InputField: NSTextField!
    @IBOutlet weak var test6ExpectedOutputField: NSTextField!

    
    let processingQueue: NSOperationQueue = NSOperationQueue()
    
    override var windowNibName: String? {
        return "EditorWindowController"
    }
    
    override func windowDidLoad() {
        super.windowDidLoad()

        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.
        
        // from http://stackoverflow.com/questions/19801601/nstextview-with-smart-quotes-disabled-still-replaces-quotes
        schemeDefinitionView.automaticQuoteSubstitutionEnabled = false
        bestGuessView.automaticQuoteSubstitutionEnabled = false

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
    
    func textDidChange(notification: NSNotification) {
        // NSTextView text changed
        print("@@@@@@@@@@@@@@@@@@@ textDidChange")
        runCodeFromEditPane()
    }
    
    override func controlTextDidChange(aNotification: NSNotification) {
        // NSTextField text changed
        print("@@@@@@@@@@@@@@@@@@@ controlTextDidChange")
        runCodeFromEditPane()
    }
    
    func runCodeFromEditPane() {
        
        // The text in the code pane changed!  Launch a new Scheme task to evaluate the new expression...
        
        let processTest1 = !test1InputField.stringValue.isEmpty && !test1ExpectedOutputField.stringValue.isEmpty
        let processTest2 = !test2InputField.stringValue.isEmpty && !test2ExpectedOutputField.stringValue.isEmpty
        let processTest3 = !test3InputField.stringValue.isEmpty && !test3ExpectedOutputField.stringValue.isEmpty
        let processTest4 = !test4InputField.stringValue.isEmpty && !test4ExpectedOutputField.stringValue.isEmpty
        let processTest5 = !test5InputField.stringValue.isEmpty && !test5ExpectedOutputField.stringValue.isEmpty
        let processTest6 = !test6InputField.stringValue.isEmpty && !test6ExpectedOutputField.stringValue.isEmpty

        
        
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
        let query_file_test4 = "barliman-query-test4.scm"
        let query_file_test5 = "barliman-query-test5.scm"
        let query_file_test6 = "barliman-query-test6.scm"
        let query_file_alltests = "barliman-query-alltests.scm"

        let mk_vicare_path_string = mk_vicare_path as! String
        let mk_path_string = mk_path as! String
        let interp_path_string = interp_path as! String

        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"
        let load_interp_string: String = "(load \"\( interp_path_string )\")"

        let queryPrefix = "(write (run 1 (q) (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) (evalo `(begin "
        let querySuffix = "))) )"
        let definitionText = (schemeDefinitionView.textStorage as NSAttributedString!).string
        
        
        let querySimple: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " ,Z) q" + querySuffix

        
        let queryTest1: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " " + test1InputField.stringValue + ") " + test1ExpectedOutputField.stringValue + querySuffix

        let queryTest2: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " " + test2InputField.stringValue + ") " + test2ExpectedOutputField.stringValue + querySuffix

        let queryTest3: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " " + test3InputField.stringValue + ") " + test3ExpectedOutputField.stringValue + querySuffix

        let queryTest4: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " " + test4InputField.stringValue + ") " + test4ExpectedOutputField.stringValue + querySuffix

        let queryTest5: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " " + test5InputField.stringValue + ") " + test5ExpectedOutputField.stringValue + querySuffix

        let queryTest6: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            queryPrefix + definitionText + " " + test6InputField.stringValue + ") " + test6ExpectedOutputField.stringValue + querySuffix
        
        
        let in1 = (processTest1 ? test1InputField.stringValue : "")
        let in2 = (processTest2 ? test2InputField.stringValue : "")
        let in3 = (processTest3 ? test3InputField.stringValue : "")
        let in4 = (processTest4 ? test4InputField.stringValue : "")
        let in5 = (processTest5 ? test5InputField.stringValue : "")
        let in6 = (processTest6 ? test6InputField.stringValue : "")

        let out1 = (processTest1 ? test1ExpectedOutputField.stringValue : "")
        let out2 = (processTest2 ? test2ExpectedOutputField.stringValue : "")
        let out3 = (processTest3 ? test3ExpectedOutputField.stringValue : "")
        let out4 = (processTest4 ? test4ExpectedOutputField.stringValue : "")
        let out5 = (processTest5 ? test5ExpectedOutputField.stringValue : "")
        let out6 = (processTest6 ? test6ExpectedOutputField.stringValue : "")
        
        let allTestInputs = in1 + " "
                          + in2 + " "
                          + in3 + " "
                          + in4 + " "
                          + in5 + " "
                          + in6 + " "
        let allTestOutputs = out1 + " "
                           + out2 + " "
                           + out3 + " "
                           + out4 + " "
                           + out5 + " "
                           + out6 + " "
        
        let queryAllTests: String = load_mk_vicare_string +
            load_mk_string +
            load_interp_string +
            "(write (let ((ans (run 1 (defn) (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) (== `" + definitionText + " defn) (evalo `(begin ,defn (list " + allTestInputs + ")) (list " +  allTestOutputs + ")" + "))))) (if (null? ans) 'fail (car ans))) )"

        
        
        print("querySimple = \n\( querySimple )\n")
        print("queryTest1 = \n\( queryTest1 )\n")
        print("queryTest2 = \n\( queryTest2 )\n")
        print("queryTest3 = \n\( queryTest3 )\n")
        print("queryTest4 = \n\( queryTest4 )\n")
        print("queryTest5 = \n\( queryTest5 )\n")
        print("queryTest6 = \n\( queryTest6 )\n")
        print("queryAllTests = \n\( queryAllTests )\n")
        
        
        
        var pathSimple: NSURL
        pathSimple = NSURL()
        
        var pathTest1: NSURL
        pathTest1 = NSURL()
        
        var pathTest2: NSURL
        pathTest2 = NSURL()
        
        var pathTest3: NSURL
        pathTest3 = NSURL()

        var pathTest4: NSURL
        pathTest4 = NSURL()

        var pathTest5: NSURL
        pathTest5 = NSURL()

        var pathTest6: NSURL
        pathTest6 = NSURL()
        
        var pathAllTests: NSURL
        pathAllTests = NSURL()


        
        // write the temporary file containing the query to the user's Document directory.  This seems a bit naughty.  Where is the right place to put this?  In ~/.barliman, perhaps?
        if let dir = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true).first {
            
            pathSimple = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_simple)
            pathTest1 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test1)
            pathTest2 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test2)
            pathTest3 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test3)
            pathTest4 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test4)
            pathTest5 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test5)
            pathTest6 = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_test6)
            pathAllTests = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file_alltests)

            
            // write the query files
            do {
                try querySimple.writeToURL(pathSimple, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest1.writeToURL(pathTest1, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest2.writeToURL(pathTest2, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest3.writeToURL(pathTest3, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest4.writeToURL(pathTest4, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest5.writeToURL(pathTest5, atomically: false, encoding: NSUTF8StringEncoding)
                try queryTest6.writeToURL(pathTest6, atomically: false, encoding: NSUTF8StringEncoding)
                try queryAllTests.writeToURL(pathAllTests, atomically: false, encoding: NSUTF8StringEncoding)

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

        var schemeScriptPathStringTest4: String = ""
        schemeScriptPathStringTest4 = pathTest4.path!

        var schemeScriptPathStringTest5: String = ""
        schemeScriptPathStringTest5 = pathTest5.path!

        var schemeScriptPathStringTest6: String = ""
        schemeScriptPathStringTest6 = pathTest6.path!
        
        var schemeScriptPathStringAllTests: String = ""
        schemeScriptPathStringAllTests = pathAllTests.path!


        
        // create the operations that will be placed in the operation queue
        let runSchemeOpSimple: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringSimple, taskType: "simple")
        
        let runSchemeOpTest1: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest1, taskType: "test1")
        
        let runSchemeOpTest2: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest2, taskType: "test2")
        
        let runSchemeOpTest3: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest3, taskType: "test3")
        
        let runSchemeOpTest4: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest4, taskType: "test4")

        let runSchemeOpTest5: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest5, taskType: "test5")

        let runSchemeOpTest6: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringTest6, taskType: "test6")
        
        let runSchemeOpAllTests: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringAllTests, taskType: "allTests")



        // wait until the previous operations kill their tasks and finish, before adding the new operations
        //
        // This operation seems expensive.  Barliman seems to work okay without this call.  Need we worry about a race condition here?
        //
        // processingQueue.waitUntilAllOperationsAreFinished()
        
        
        // now that the previous operations have completed, safe to add the new operations
        processingQueue.addOperation(runSchemeOpSimple)
        if processTest1 {
            print("queuing test1")
            processingQueue.addOperation(runSchemeOpTest1)
        } else {
            test1InputField.textColor = NSColor.blackColor()
            test1ExpectedOutputField.textColor = NSColor.blackColor()
        }
        if processTest2 {
            print("queuing test2")
            processingQueue.addOperation(runSchemeOpTest2)
        } else {
            test2InputField.textColor = NSColor.blackColor()
            test2ExpectedOutputField.textColor = NSColor.blackColor()
        }
        if processTest3 {
            print("queuing test3")
            processingQueue.addOperation(runSchemeOpTest3)
        } else {
            test3InputField.textColor = NSColor.blackColor()
            test3ExpectedOutputField.textColor = NSColor.blackColor()
        }
        if processTest4 {
            print("queuing test4")
            processingQueue.addOperation(runSchemeOpTest4)
        } else {
            test4InputField.textColor = NSColor.blackColor()
            test4ExpectedOutputField.textColor = NSColor.blackColor()
        }
        if processTest5 {
            print("queuing test5")
            processingQueue.addOperation(runSchemeOpTest5)
        } else {
            test5InputField.textColor = NSColor.blackColor()
            test5ExpectedOutputField.textColor = NSColor.blackColor()
        }
        if processTest6 {
            print("queuing test6")
            processingQueue.addOperation(runSchemeOpTest6)
        } else {
            test6InputField.textColor = NSColor.blackColor()
            test6ExpectedOutputField.textColor = NSColor.blackColor()
        }
        processingQueue.addOperation(runSchemeOpAllTests)
    }
}
