//
//  EditorWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

class EditorWindowController: NSWindowController {

    // Making these views weak references seems to cause a runtime error.  Why?
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

    @IBOutlet weak var schemeDefinitionSpinner: NSProgressIndicator!
    @IBOutlet weak var bestGuessSpinner: NSProgressIndicator!
    @IBOutlet weak var test1Spinner: NSProgressIndicator!
    @IBOutlet weak var test2Spinner: NSProgressIndicator!
    @IBOutlet weak var test3Spinner: NSProgressIndicator!
    @IBOutlet weak var test4Spinner: NSProgressIndicator!
    @IBOutlet weak var test5Spinner: NSProgressIndicator!
    @IBOutlet weak var test6Spinner: NSProgressIndicator!


    var runCodeFromEditPaneTimer: NSTimer?
    
    var semanticsWindowController: SemanticsWindowController?
    
    // keep track of the operation that runs all the tests together, in case we need to cancel it
    var schemeOperationAllTests: RunSchemeOperation?
    
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
        
        // For whatever reason, the tabbing from Test 3 Expected Output doesn't got to Test 4 Input
        test3ExpectedOutputField.nextKeyView = test4InputField

    }

    func cleanup() {
        // application is about to quit -- clean up!
        
        print("cleaning up!")
        
        runCodeFromEditPaneTimer?.invalidate()
        
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
        
        setupRunCodeFromEditPaneTimer()
    }
    
    override func controlTextDidChange(aNotification: NSNotification) {
        // NSTextField text changed
        print("@@@@@@@@@@@@@@@@@@@ controlTextDidChange")
        
        setupRunCodeFromEditPaneTimer()
    }
    
    func setupRunCodeFromEditPaneTimer() {
        runCodeFromEditPaneTimer?.invalidate()

        runCodeFromEditPaneTimer = NSTimer.scheduledTimerWithTimeInterval(1, target:self, selector: #selector(EditorWindowController.runCodeFromEditPane), userInfo: nil, repeats: false)
    }
    
    func makeQueryFileString(defns: String,
                             body: String,
                             expectedOut: String,
                             interp_string: String,
                             mk_vicare_path_string: String,
                             mk_path_string: String,
                             simple: Bool) -> String {
        
        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"
        let parse_ans_string: String = "(define parse-ans (run 1 (q)" + "\n" +
        " (let ((g1 (gensym \"g1\")) (g2 (gensym \"g2\")) (g3 (gensym \"g3\")) (g4 (gensym \"g4\")) (g5 (gensym \"g5\")) (g6 (gensym \"g6\")) (g7 (gensym \"g7\")) (g8 (gensym \"g8\")) (g9 (gensym \"g9\")) (g10 (gensym \"g10\")) (g11 (gensym \"g11\")) (g12 (gensym \"g12\")) (g13 (gensym \"g13\")) (g14 (gensym \"g14\")) (g15 (gensym \"g15\")) (g16 (gensym \"g16\")) (g17 (gensym \"g17\")) (g18 (gensym \"g18\")) (g19 (gensym \"g19\")) (g20 (gensym \"g20\")))" + "\n" +
        "(fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _) (parseo `(begin \( defns ) \( body )))))))"
        
        let parse_with_fake_defns_ans_string: String = "(define parse-ans (run 1 (q)" + "\n" +
            " (let ((g1 (gensym \"g1\")) (g2 (gensym \"g2\")) (g3 (gensym \"g3\")) (g4 (gensym \"g4\")) (g5 (gensym \"g5\")) (g6 (gensym \"g6\")) (g7 (gensym \"g7\")) (g8 (gensym \"g8\")) (g9 (gensym \"g9\")) (g10 (gensym \"g10\")) (g11 (gensym \"g11\")) (g12 (gensym \"g12\")) (g13 (gensym \"g13\")) (g14 (gensym \"g14\")) (g15 (gensym \"g15\")) (g16 (gensym \"g16\")) (g17 (gensym \"g17\")) (g18 (gensym \"g18\")) (g19 (gensym \"g19\")) (g20 (gensym \"g20\")))" + "\n" +
            " (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _) (fresh (names dummy-expr) (extract-nameso `( \( defns ) ) names) (parseo `((lambda ,names \( body )) ,dummy-expr)))))))"

        
        
        // get the path to the application's bundle, so we can load the query string files
        let bundle = NSBundle.mainBundle()
        
        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let interp_eval_query_string_part_1: String? = bundle.pathForResource("interp-eval-query-string-part-1", ofType: "swift", inDirectory: "mk-and-rel-interp")
        let interp_eval_query_string_part_2: String? = bundle.pathForResource("interp-eval-query-string-part-2", ofType: "swift", inDirectory: "mk-and-rel-interp")

        let eval_string_part_1 : String
        do
        {
            eval_string_part_1 = try String(contentsOfFile: interp_eval_query_string_part_1!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load eval_string_part_1\n")
            eval_string_part_1 = ""
        }
        
        let eval_string_part_2 : String
        do
        {
            eval_string_part_2 = try String(contentsOfFile: interp_eval_query_string_part_2!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load eval_string_part_2\n")
            eval_string_part_2 = ""
        }

        let eval_string = eval_string_part_1 + "\n" +
                          "        (== `( \( defns ) ) defn-list)" + "\n" +
                          eval_string_part_2 + "\n" +
                          " (evalo `(begin \( defns ) \( body )) \( expectedOut )))))"
        
        
        let write_ans_string: String = "(write (if (null? parse-ans) 'parse-error \( eval_string )))"
        
        let full_string: String = load_mk_vicare_string + "\n" +
                                  load_mk_string + "\n" +
                                  interp_string + "\n" +
                                  (simple ? ";; simple query" : ";; individual test query") + "\n" +
                                  (simple ? parse_ans_string : parse_with_fake_defns_ans_string) + "\n" +
                                  write_ans_string
      
        print("query string:\n \( full_string )\n")
        
        return full_string
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

        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"

        let interp_string: String = semanticsWindowController!.getInterpreterCode()
        
        let definitionText = (schemeDefinitionView.textStorage as NSAttributedString!).string
        
        
        let querySimple: String =
            makeQueryFileString(definitionText,
                                body: ",_",
                                expectedOut: "q",
                                interp_string: interp_string,
                                mk_vicare_path_string: mk_vicare_path_string,
                                mk_path_string: mk_path_string,
                                simple: true)
        
        let queryTest1: String =
            (processTest1 ?
                makeQueryFileString(definitionText,
                    body: test1InputField.stringValue,
                    expectedOut: test1ExpectedOutputField.stringValue,
                    interp_string: interp_string,
                    mk_vicare_path_string: mk_vicare_path_string,
                    mk_path_string: mk_path_string,
                    simple: false)
                : "")

        let queryTest2: String =
            (processTest2 ?
                makeQueryFileString(definitionText,
                    body: test2InputField.stringValue,
                    expectedOut: test2ExpectedOutputField.stringValue,
                    interp_string: interp_string,
                    mk_vicare_path_string: mk_vicare_path_string,
                    mk_path_string: mk_path_string,
                    simple: false)
                : "")

        let queryTest3: String =
            (processTest3 ?
                makeQueryFileString(definitionText,
                    body: test3InputField.stringValue,
                    expectedOut: test3ExpectedOutputField.stringValue,
                    interp_string: interp_string,
                    mk_vicare_path_string: mk_vicare_path_string,
                    mk_path_string: mk_path_string,
                    simple: false)
                : "")
        
        let queryTest4: String =
            (processTest4 ?
                makeQueryFileString(definitionText,
                    body: test4InputField.stringValue,
                    expectedOut: test4ExpectedOutputField.stringValue,
                    interp_string: interp_string,
                    mk_vicare_path_string: mk_vicare_path_string,
                    mk_path_string: mk_path_string,
                    simple: false)
                : "")

        let queryTest5: String =
            (processTest5 ?
                makeQueryFileString(definitionText,
                    body: test5InputField.stringValue,
                    expectedOut: test5ExpectedOutputField.stringValue,
                    interp_string: interp_string,
                    mk_vicare_path_string: mk_vicare_path_string,
                    mk_path_string: mk_path_string,
                    simple: false)
                : "")
        
        let queryTest6: String =
            (processTest6 ?
                makeQueryFileString(definitionText,
                    body: test6InputField.stringValue,
                    expectedOut: test6ExpectedOutputField.stringValue,
                    interp_string: interp_string,
                    mk_vicare_path_string: mk_vicare_path_string,
                    mk_path_string: mk_path_string,
                    simple: false)
                : "")

        
        
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
        
        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let interp_alltests_query_string_part_1: String? = bundle.pathForResource("interp-alltests-query-string-part-1", ofType: "swift", inDirectory: "mk-and-rel-interp")
        let interp_alltests_query_string_part_2: String? = bundle.pathForResource("interp-alltests-query-string-part-2", ofType: "swift", inDirectory: "mk-and-rel-interp")
        
        let alltests_string_part_1 : String
        do
        {
            alltests_string_part_1 = try String(contentsOfFile: interp_alltests_query_string_part_1!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load alltests_string_part_1\n")
            alltests_string_part_1 = ""
        }
        
        let alltests_string_part_2 : String
        do
        {
            alltests_string_part_2 = try String(contentsOfFile: interp_alltests_query_string_part_2!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load alltests_string_part_2\n")
            alltests_string_part_2 = ""
        }

        
        let allTestWriteString = alltests_string_part_1 + "\n" +
        "        (== `( \( definitionText ) ) defn-list)" + "\n" + "\n" +
            alltests_string_part_2 + "\n" +
            "(== `(" +
            definitionText +
            ") defns) (appendo defns `((list " +
            allTestInputs +
            ")) begin-body) (evalo `(begin . ,begin-body) (list " +
            allTestOutputs +
            ")" +
            ")))))) (if (null? ans) 'fail `(begin ,@(car ans) ...)) ))"
        
        let queryAllTests: String = load_mk_vicare_string +
            load_mk_string +
            interp_string + "\n" +
            ";; allTestWriteString" + "\n" +
            allTestWriteString
        
        print("queryAllTests string:\n \( queryAllTests )\n")

        
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
        
        schemeOperationAllTests = runSchemeOpAllTests



        // wait until the previous operations kill their tasks and finish, before adding the new operations
        //
        // This operation seems expensive.  Barliman seems to work okay without this call.  Need we worry about a race condition here?
        //
        processingQueue.waitUntilAllOperationsAreFinished()
        
        
        // now that the previous operations have completed, safe to add the new operations
        processingQueue.addOperation(runSchemeOpAllTests)
        
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
    }
}
