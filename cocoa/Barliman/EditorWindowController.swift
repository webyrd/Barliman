//
//  EditorWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

class EditorWindowController: NSWindowController, NSSplitViewDelegate, NSControlTextEditingDelegate, NSTextDelegate {

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

    @IBOutlet weak var definitionStatusLabel: NSTextField!
    @IBOutlet weak var test1StatusLabel: NSTextField!
    @IBOutlet weak var test2StatusLabel: NSTextField!
    @IBOutlet weak var test3StatusLabel: NSTextField!
    @IBOutlet weak var test4StatusLabel: NSTextField!
    @IBOutlet weak var test5StatusLabel: NSTextField!
    @IBOutlet weak var test6StatusLabel: NSTextField!
    @IBOutlet weak var bestGuessStatusLabel: NSTextField!

    @IBOutlet weak var definitionAndBestGuessSplitView: NSSplitView!


    var runCodeFromEditPaneTimer: Timer?

    var semanticsWindowController: SemanticsWindowController?

    // keep track of the operation that runs all the tests together, in case we need to cancel it
    var schemeOperationAllTests: RunSchemeOperation?

    let processingQueue: OperationQueue = OperationQueue()
    
    static func fontName() -> String {
        return "Monaco"
    }
    
    static func fontSize() -> CGFloat {
        return 14
    }

    static func defaultColor() -> NSColor {
        return NSColor.black
    }
    
    
    override var windowNibName: String? {
        return "EditorWindowController"
    }

    override func windowDidLoad() {
        super.windowDidLoad()

        // Implement this method to handle any initialization after your window controller's window has been loaded from its nib file.

        // from http://stackoverflow.com/questions/19801601/nstextview-with-smart-quotes-disabled-still-replaces-quotes
        schemeDefinitionView.isAutomaticQuoteSubstitutionEnabled = false
        bestGuessView.isAutomaticQuoteSubstitutionEnabled = false

        // For whatever reason, the tabbing from Test 3 Expected Output doesn't got to Test 4 Input
        test3ExpectedOutputField.nextKeyView = test4InputField
        
        let defaultFontName = EditorWindowController.fontName()
        let defaultFontSize = EditorWindowController.fontSize()
        let font = NSFont(name: defaultFontName, size: defaultFontSize)
        
        schemeDefinitionView.font = NSFont(name: defaultFontName, size: defaultFontSize)
        bestGuessView.font = NSFont(name: defaultFontName, size: defaultFontSize)
        
        test1InputField.font = font
        test2InputField.font = font
        test3InputField.font = font
        test4InputField.font = font
        test5InputField.font = font
        test6InputField.font = font
        
        test1ExpectedOutputField.font = font
        test2ExpectedOutputField.font = font
        test3ExpectedOutputField.font = font
        test4ExpectedOutputField.font = font
        test5ExpectedOutputField.font = font
        test6ExpectedOutputField.font = font
        
        // from http://stackoverflow.com/questions/28001996/setting-minimum-width-of-nssplitviews
        self.definitionAndBestGuessSplitView.delegate = self
    }
    
    // from http://stackoverflow.com/questions/28001996/setting-minimum-width-of-nssplitviews
    func splitView(_ splitView: NSSplitView, constrainMinCoordinate proposedMinimumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat {
        return proposedMinimumPosition + 30
    }
    
    // from http://stackoverflow.com/questions/28001996/setting-minimum-width-of-nssplitviews
    func splitView(_ splitView: NSSplitView, constrainMaxCoordinate proposedMaximumPosition: CGFloat, ofSubviewAt dividerIndex: Int) -> CGFloat {
        return proposedMaximumPosition - 50
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
    
    func textDidChange(_ notification: Notification) {
        // NSTextView text changed
        print("@@@@@@@@@@@@@@@@@@@ textDidChange")

        setupRunCodeFromEditPaneTimer()
    }

    func controlTextDidChange(_ aNotification: Notification) {
        // NSTextField text changed
        print("@@@@@@@@@@@@@@@@@@@ controlTextDidChange")

        setupRunCodeFromEditPaneTimer()
    }

    func setupRunCodeFromEditPaneTimer() {
        runCodeFromEditPaneTimer?.invalidate()

        runCodeFromEditPaneTimer = .scheduledTimer(timeInterval: 1, target:self, selector: #selector(runCodeFromEditPane), userInfo: nil, repeats: false)
    }

    func makeQuerySimpleForMondoSchemeFileString(_ interp_string: String,
                                                 mk_vicare_path_string: String,
                                                 mk_path_string: String) -> String {

        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"

        let definitionText = schemeDefinitionView.textStorage!.string

        let querySimple: String =   makeQueryString(definitionText,
                                                    body: ",_",
                                                    expectedOut: "q",
                                                    simple: true,
                                                    name: "-simple")


        let full_string: String = load_mk_vicare_string + "\n" +
                                  load_mk_string + "\n" +
                                  interp_string + "\n" +
                                  querySimple

        return full_string
    }


    func makeAllTestsQueryString() -> String {

        let processTest1 = !test1InputField.stringValue.isEmpty && !test1ExpectedOutputField.stringValue.isEmpty
        let processTest2 = !test2InputField.stringValue.isEmpty && !test2ExpectedOutputField.stringValue.isEmpty
        let processTest3 = !test3InputField.stringValue.isEmpty && !test3ExpectedOutputField.stringValue.isEmpty
        let processTest4 = !test4InputField.stringValue.isEmpty && !test4ExpectedOutputField.stringValue.isEmpty
        let processTest5 = !test5InputField.stringValue.isEmpty && !test5ExpectedOutputField.stringValue.isEmpty
        let processTest6 = !test6InputField.stringValue.isEmpty && !test6ExpectedOutputField.stringValue.isEmpty

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

        let definitionText = schemeDefinitionView.textStorage!.string

        // get the path to the application's bundle, so we can load the query string files
        let bundle = Bundle.main

        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let interp_alltests_query_string_part_1: String? = bundle.path(forResource: "interp-alltests-query-string-part-1", ofType: "swift", inDirectory: "mk-and-rel-interp")
        let interp_alltests_query_string_part_2: String? = bundle.path(forResource: "interp-alltests-query-string-part-2", ofType: "swift", inDirectory: "mk-and-rel-interp")

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

        let eval_flags_fast = "(set! allow-incomplete-search? #t)"
        let eval_flags_complete = "(set! allow-incomplete-search? #f)"
        let eval_string_fast = "(begin \( eval_flags_fast ) (results))"
        let eval_string_complete = "(begin \( eval_flags_complete ) (results))"

        let allTestWriteString = "(define (ans-allTests)\n" +
                                 "  (define (results)\n" +
                                 alltests_string_part_1 + "\n" +
            "        (== `( \( definitionText ) ) defn-list)" + "\n" + "\n" +
            alltests_string_part_2 + "\n" +
            "(== `(" +
            definitionText +
            ") defns) (appendo defns `(((lambda x x) " +
            allTestInputs +
            ")) begin-body) (evalo `(begin . ,begin-body) (list " +
            allTestOutputs +
            ")" +
        ")))))\n" +
        "(let ((results-fast \( eval_string_fast )))\n" +
        "  (if (null? results-fast)\n" +
        "    \( eval_string_complete )\n" +
        "    results-fast)))"

        let fullString: String = ";; allTests" + "\n" + allTestWriteString

        print("queryAllTests string:\n \( fullString )\n")

        return fullString
    }

    func makeQueryString(_ defns: String,
                         body: String,
                         expectedOut: String,
                         simple: Bool,
                         name: String) -> String {

        let parse_ans_string: String = "(define (parse-ans\( name )) (run 1 (q)" + "\n" +
            " (let ((g1 (gensym \"g1\")) (g2 (gensym \"g2\")) (g3 (gensym \"g3\")) (g4 (gensym \"g4\")) (g5 (gensym \"g5\")) (g6 (gensym \"g6\")) (g7 (gensym \"g7\")) (g8 (gensym \"g8\")) (g9 (gensym \"g9\")) (g10 (gensym \"g10\")) (g11 (gensym \"g11\")) (g12 (gensym \"g12\")) (g13 (gensym \"g13\")) (g14 (gensym \"g14\")) (g15 (gensym \"g15\")) (g16 (gensym \"g16\")) (g17 (gensym \"g17\")) (g18 (gensym \"g18\")) (g19 (gensym \"g19\")) (g20 (gensym \"g20\")))" + "\n" +
            "(fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _) (parseo `(begin \( defns ) \( body )))))))"

        let parse_with_fake_defns_ans_string: String = "(define (parse-ans\( name )) (run 1 (q)" + "\n" +
            " (let ((g1 (gensym \"g1\")) (g2 (gensym \"g2\")) (g3 (gensym \"g3\")) (g4 (gensym \"g4\")) (g5 (gensym \"g5\")) (g6 (gensym \"g6\")) (g7 (gensym \"g7\")) (g8 (gensym \"g8\")) (g9 (gensym \"g9\")) (g10 (gensym \"g10\")) (g11 (gensym \"g11\")) (g12 (gensym \"g12\")) (g13 (gensym \"g13\")) (g14 (gensym \"g14\")) (g15 (gensym \"g15\")) (g16 (gensym \"g16\")) (g17 (gensym \"g17\")) (g18 (gensym \"g18\")) (g19 (gensym \"g19\")) (g20 (gensym \"g20\")))" + "\n" +
            " (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _) (fresh (names dummy-expr) (extract-nameso `( \( defns ) ) names) (parseo `((lambda ,names \( body )) ,dummy-expr)))))))"



        // get the path to the application's bundle, so we can load the query string files
        let bundle = Bundle.main

        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let interp_eval_query_string_part_1: String? = bundle.path(forResource: "interp-eval-query-string-part-1", ofType: "swift", inDirectory: "mk-and-rel-interp")
        let interp_eval_query_string_part_2: String? = bundle.path(forResource: "interp-eval-query-string-part-2", ofType: "swift", inDirectory: "mk-and-rel-interp")

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

        let eval_flags_fast = "(set! allow-incomplete-search? #t)"
        let eval_flags_complete = "(set! allow-incomplete-search? #f)"

        let eval_string_fast = "(begin \( eval_flags_fast ) \( eval_string ))"
        let eval_string_complete = "(begin \( eval_flags_complete ) \( eval_string ))"
        let eval_string_both = "(let ((results-fast \( eval_string_fast )))\n" +
                               "  (if (null? results-fast)\n" +
                               "    \( eval_string_complete )\n" +
                               "     results-fast))"

        let define_ans_string: String = "(define (query-val\( name ))" + "\n" +
                                        "  (if (null? (parse-ans\( name )))" + "\n" +
                                        "      'parse-error" + "\n" +
                                        "      \( eval_string_both )))"

        let full_string: String = (simple ? ";; simple query" : ";; individual test query") + "\n\n" +
                                  (simple ? parse_ans_string : parse_with_fake_defns_ans_string) + "\n\n" +
                                  define_ans_string + "\n\n"

        print("query string:\n \( full_string )\n")

        return full_string
    }


    @objc func runCodeFromEditPane() {

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
        let bundle = Bundle.main

        let mk_vicare_path: NSString? = bundle.path(forResource: "mk-vicare", ofType: "scm", inDirectory: "mk-and-rel-interp/mk") as NSString?
        let mk_path: NSString? = bundle.path(forResource: "mk", ofType: "scm", inDirectory: "mk-and-rel-interp/mk") as NSString?


        // write the Scheme code containing the miniKanren query to a temp file
        let query_simple_for_mondo_scheme_file = "barliman-query-simple-for-mondo-scheme-file.scm"

        // files that load query code
        let new_query_file_simple = "barliman-new-query-simple.scm"
        let new_query_file_test1 = "barliman-new-query-test1.scm"
        let new_query_file_test2 = "barliman-new-query-test2.scm"
        let new_query_file_test3 = "barliman-new-query-test3.scm"
        let new_query_file_test4 = "barliman-new-query-test4.scm"
        let new_query_file_test5 = "barliman-new-query-test5.scm"
        let new_query_file_test6 = "barliman-new-query-test6.scm"
        let new_query_file_alltests = "barliman-new-query-alltests.scm"

        // files containing the actual query code
        let new_query_file_actual_test1 = "barliman-new-query-actual-test1.scm"
        let new_query_file_actual_test2 = "barliman-new-query-actual-test2.scm"
        let new_query_file_actual_test3 = "barliman-new-query-actual-test3.scm"
        let new_query_file_actual_test4 = "barliman-new-query-actual-test4.scm"
        let new_query_file_actual_test5 = "barliman-new-query-actual-test5.scm"
        let new_query_file_actual_test6 = "barliman-new-query-actual-test6.scm"
        let new_query_file_actual_alltests = "barliman-new-query-actual-alltests.scm"


        let mk_vicare_path_string = mk_vicare_path! as String
        let mk_path_string = mk_path! as String

        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"

        let interp_string: String = semanticsWindowController!.getInterpreterCode()

        let definitionText = schemeDefinitionView.textStorage!.string


        let querySimpleForMondoSchemeContents: String = makeQuerySimpleForMondoSchemeFileString(interp_string,
                                                                                                mk_vicare_path_string: mk_vicare_path_string,
                                                                                                mk_path_string: mk_path_string)





        let newTest1ActualQueryString: String =   makeQueryString(definitionText,
                                                         body: test1InputField.stringValue,
                                                         expectedOut: test1ExpectedOutputField.stringValue,
                                                         simple: false,
                                                         name: "-test1")

        let newTest2ActualQueryString: String =   makeQueryString(definitionText,
                                                         body: test2InputField.stringValue,
                                                         expectedOut: test2ExpectedOutputField.stringValue,
                                                         simple: false,
                                                         name: "-test2")

        let newTest3ActualQueryString: String =   makeQueryString(definitionText,
                                                         body: test3InputField.stringValue,
                                                         expectedOut: test3ExpectedOutputField.stringValue,
                                                         simple: false,
                                                         name: "-test3")

        let newTest4ActualQueryString: String =   makeQueryString(definitionText,
                                                         body: test4InputField.stringValue,
                                                         expectedOut: test4ExpectedOutputField.stringValue,
                                                         simple: false,
                                                         name: "-test4")

        let newTest5ActualQueryString: String =   makeQueryString(definitionText,
                                                         body: test5InputField.stringValue,
                                                         expectedOut: test5ExpectedOutputField.stringValue,
                                                         simple: false,
                                                         name: "-test5")

        let newTest6ActualQueryString: String =   makeQueryString(definitionText,
                                                         body: test6InputField.stringValue,
                                                         expectedOut: test6ExpectedOutputField.stringValue,
                                                         simple: false,
                                                         name: "-test6")


        let newAlltestsActualQueryString = makeAllTestsQueryString()



        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let new_simple_query_template: String? = bundle.path(forResource: "barliman-new-simple-query-template", ofType: "swift", inDirectory: "mk-and-rel-interp")

        let new_simple_query_template_string : String
        do
        {
            new_simple_query_template_string = try String(contentsOfFile: new_simple_query_template!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load new_simple_query_template\n")
            new_simple_query_template_string = ""
        }



        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let new_test_query_template: String? = bundle.path(forResource: "barliman-new-test-query-template", ofType: "swift", inDirectory: "mk-and-rel-interp")

        let new_test_query_template_string : String
        do
        {
            new_test_query_template_string = try String(contentsOfFile: new_test_query_template!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load new_test_query_template\n")
            new_test_query_template_string = ""
        }



        // adapted from http://stackoverflow.com/questions/26573332/reading-a-short-text-file-to-a-string-in-swift
        let new_alltests_query_template: String? = bundle.path(forResource: "barliman-new-alltests-query-template", ofType: "swift", inDirectory: "mk-and-rel-interp")

        let new_alltests_query_template_string : String
        do
        {
            new_alltests_query_template_string = try String(contentsOfFile: new_alltests_query_template!)
        }
        catch
        {
            print("!!!!!  LOAD_ERROR -- can't load new_test_query_template\n")
            new_alltests_query_template_string = ""
        }



        let newSimpleQueryString: String
        let newTest1QueryString: String
        let newTest2QueryString: String
        let newTest3QueryString: String
        let newTest4QueryString: String
        let newTest5QueryString: String
        let newTest6QueryString: String
        let newAlltestsQueryString: String


        if let dir = NSSearchPathForDirectoriesInDomains(.documentDirectory, .allDomainsMask, true).first {

            let fullSimpleQueryForMondoSchemeFilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-query-simple-for-mondo-scheme-file.scm")
            let localSimpleQueryForMondoSchemeFilePath = fullSimpleQueryForMondoSchemeFilePath.path

            let fullNewQueryActualTest1FilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-test1.scm")
            let localNewQueryActualTest1FilePath = fullNewQueryActualTest1FilePath.path

            let fullNewQueryActualTest2FilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-test2.scm")
            let localNewQueryActualTest2FilePath = fullNewQueryActualTest2FilePath.path

            let fullNewQueryActualTest3FilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-test3.scm")
            let localNewQueryActualTest3FilePath = fullNewQueryActualTest3FilePath.path

            let fullNewQueryActualTest4FilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-test4.scm")
            let localNewQueryActualTest4FilePath = fullNewQueryActualTest4FilePath.path

            let fullNewQueryActualTest5FilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-test5.scm")
            let localNewQueryActualTest5FilePath = fullNewQueryActualTest5FilePath.path

            let fullNewQueryActualTest6FilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-test6.scm")
            let localNewQueryActualTest6FilePath = fullNewQueryActualTest6FilePath.path

            let fullNewQueryActualAlltestsFilePath = URL(fileURLWithPath: dir).appendingPathComponent("barliman-new-query-actual-alltests.scm")
            let localNewQueryActualAlltestsFilePath = fullNewQueryActualAlltestsFilePath.path



            let loadFileString =
                "(define simple-query-for-mondo-file-path \"\( localSimpleQueryForMondoSchemeFilePath )\")"

            newSimpleQueryString = loadFileString + "\n\n" + new_simple_query_template_string

            newAlltestsQueryString =
                loadFileString + "\n\n" +
                "(define actual-query-file-path \"\( localNewQueryActualAlltestsFilePath )\")" + "\n\n" +
                new_alltests_query_template_string


            func makeNewTestNQueryString(_ n: Int, actualQueryFilePath: String) -> String {
                return loadFileString + "\n\n" +
                    "(define actual-query-file-path \"\( actualQueryFilePath )\")" + "\n\n" +
                    "(define (test-query-fn) (query-val-test\( n )))" + "\n\n\n" +
                    new_test_query_template_string
            }

            newTest1QueryString = makeNewTestNQueryString(1, actualQueryFilePath: localNewQueryActualTest1FilePath)
            newTest2QueryString = makeNewTestNQueryString(2, actualQueryFilePath: localNewQueryActualTest2FilePath)
            newTest3QueryString = makeNewTestNQueryString(3, actualQueryFilePath: localNewQueryActualTest3FilePath)
            newTest4QueryString = makeNewTestNQueryString(4, actualQueryFilePath: localNewQueryActualTest4FilePath)
            newTest5QueryString = makeNewTestNQueryString(5, actualQueryFilePath: localNewQueryActualTest5FilePath)
            newTest6QueryString = makeNewTestNQueryString(6, actualQueryFilePath: localNewQueryActualTest6FilePath)

        } else {
            print("!!!!!  LOAD_ERROR -- can't find Document directory\n")

            newSimpleQueryString = ""
            newTest1QueryString = ""
            newTest2QueryString = ""
            newTest3QueryString = ""
            newTest4QueryString = ""
            newTest5QueryString = ""
            newTest6QueryString = ""
            newAlltestsQueryString = ""
        }


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


        var pathQuerySimpleForMondoSchemeFile: URL!
        var pathMondoScheme: URL!
        var pathNewSimple: URL!

        var pathNewTest1: URL!
        var pathNewTest2: URL!
        var pathNewTest3: URL!
        var pathNewTest4: URL!
        var pathNewTest5: URL!
        var pathNewTest6: URL!
        var pathNewAlltests: URL!

        var pathNewActualTest1: URL!
        var pathNewActualTest2: URL!
        var pathNewActualTest3: URL!
        var pathNewActualTest4: URL!
        var pathNewActualTest5: URL!
        var pathNewActualTest6: URL!
        var pathNewActualAlltests: URL!


        // write the temporary file containing the query to the user's Document directory.  This seems a bit naughty.  Where is the right place to put this?  In ~/.barliman, perhaps?
        if let dir = NSSearchPathForDirectoriesInDomains(.documentDirectory, .allDomainsMask, true).first {

            pathQuerySimpleForMondoSchemeFile = URL(fileURLWithPath: dir).appendingPathComponent(query_simple_for_mondo_scheme_file)


            pathNewSimple = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_simple)

            pathNewTest1 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_test1)
            pathNewTest2 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_test2)
            pathNewTest3 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_test3)
            pathNewTest4 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_test4)
            pathNewTest5 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_test5)
            pathNewTest6 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_test6)
            pathNewAlltests = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_alltests)

            pathNewActualTest1 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_test1)
            pathNewActualTest2 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_test2)
            pathNewActualTest3 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_test3)
            pathNewActualTest4 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_test4)
            pathNewActualTest5 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_test5)
            pathNewActualTest6 = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_test6)
            pathNewActualAlltests = URL(fileURLWithPath: dir).appendingPathComponent(new_query_file_actual_alltests)

            // write the query files
            do {

                try querySimpleForMondoSchemeContents.write(to: pathQuerySimpleForMondoSchemeFile, atomically: false, encoding: String.Encoding.utf8)


                try newSimpleQueryString.write(to: pathNewSimple, atomically: false, encoding: String.Encoding.utf8)

                try newTest1QueryString.write(to: pathNewTest1, atomically: false, encoding: String.Encoding.utf8)
                try newTest2QueryString.write(to: pathNewTest2, atomically: false, encoding: String.Encoding.utf8)
                try newTest3QueryString.write(to: pathNewTest3, atomically: false, encoding: String.Encoding.utf8)
                try newTest4QueryString.write(to: pathNewTest4, atomically: false, encoding: String.Encoding.utf8)
                try newTest5QueryString.write(to: pathNewTest5, atomically: false, encoding: String.Encoding.utf8)
                try newTest6QueryString.write(to: pathNewTest6, atomically: false, encoding: String.Encoding.utf8)
                try newAlltestsQueryString.write(to: pathNewAlltests, atomically: false, encoding: String.Encoding.utf8)

                try newTest1ActualQueryString.write(to: pathNewActualTest1, atomically: false, encoding: String.Encoding.utf8)
                try newTest2ActualQueryString.write(to: pathNewActualTest2, atomically: false, encoding: String.Encoding.utf8)
                try newTest3ActualQueryString.write(to: pathNewActualTest3, atomically: false, encoding: String.Encoding.utf8)
                try newTest4ActualQueryString.write(to: pathNewActualTest4, atomically: false, encoding: String.Encoding.utf8)
                try newTest5ActualQueryString.write(to: pathNewActualTest5, atomically: false, encoding: String.Encoding.utf8)
                try newTest6ActualQueryString.write(to: pathNewActualTest6, atomically: false, encoding: String.Encoding.utf8)
                try newAlltestsActualQueryString.write(to: pathNewActualAlltests, atomically: false, encoding: String.Encoding.utf8)
            }
            catch {
                // this error handling could be better!  :)
                print("couldn't write to query files")
            }
        }


        // paths to the Schemes file containing the miniKanren query
        let schemeScriptPathStringQuerySimpleForMondoScheme = pathQuerySimpleForMondoSchemeFile.path


        let schemeScriptPathStringNewSimple = pathNewSimple.path

        let schemeScriptPathStringNewTest1 = pathNewTest1.path
        let schemeScriptPathStringNewTest2 = pathNewTest2.path
        let schemeScriptPathStringNewTest3 = pathNewTest3.path
        let schemeScriptPathStringNewTest4 = pathNewTest4.path
        let schemeScriptPathStringNewTest5 = pathNewTest5.path
        let schemeScriptPathStringNewTest6 = pathNewTest6.path
        let schemeScriptPathStringNewAlltests = pathNewAlltests.path

        let schemeScriptPathStringNewActualTest1 = pathNewActualTest1.path
        let schemeScriptPathStringNewActualTest2 = pathNewActualTest2.path
        let schemeScriptPathStringNewActualTest3 = pathNewActualTest3.path
        let schemeScriptPathStringNewActualTest4 = pathNewActualTest4.path
        let schemeScriptPathStringNewActualTest5 = pathNewActualTest5.path
        let schemeScriptPathStringNewActualTest6 = pathNewActualTest6.path
        let schemeScriptPathStringNewActualAlltests = pathNewActualAlltests.path


        // create the operations that will be placed in the operation queue

        
        print("==== pathQuerySimpleForMondoSchemeFile: \( String(describing: pathQuerySimpleForMondoSchemeFile) )")
        print("==== pathSimple: \( String(describing: pathNewSimple) )")
        print("==== schemeScriptPathStringNewSimple: \( String(describing: schemeScriptPathStringNewSimple) )")
        print("==== pathNewTest1: \( String(describing: pathNewTest1) )")
        print("==== pathNewActualTest1: \( String(describing: pathNewActualTest1) )")
        
        
        let runSchemeOpSimple = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewSimple, taskType: "simple")

        let runSchemeOpTest1 = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewTest1, taskType: "test1")

        let runSchemeOpTest2 = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewTest2, taskType: "test2")

        let runSchemeOpTest3 = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewTest3, taskType: "test3")

        let runSchemeOpTest4 = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewTest4, taskType: "test4")

        let runSchemeOpTest5 = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewTest5, taskType: "test5")

        let runSchemeOpTest6 = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewTest6, taskType: "test6")

        let runSchemeOpAllTests = RunSchemeOperation(editorWindowController: self, schemeScriptPathString: schemeScriptPathStringNewAlltests, taskType: "allTests")



        schemeOperationAllTests = runSchemeOpAllTests



        // wait until the previous operations kill their tasks and finish, before adding the new operations
        //
        // This operation seems expensive.  Barliman seems to work okay without this call.  Need we worry about a race condition here?
        //
        processingQueue.waitUntilAllOperationsAreFinished()


        // now that the previous operations have completed, safe to add the new operations
        processingQueue.addOperation(runSchemeOpAllTests)

        processingQueue.addOperation(runSchemeOpSimple)

        func resetTestUI(_ statusLabel: NSTextField, inputField: NSTextField, outputField: NSTextField) {
            statusLabel.stringValue = ""
            inputField.textColor = EditorWindowController.defaultColor()
            outputField.textColor = EditorWindowController.defaultColor()
        }

        if processTest1 {
            print("queuing test1")
            processingQueue.addOperation(runSchemeOpTest1)
        } else {
            resetTestUI(test1StatusLabel, inputField: test1InputField, outputField: test1ExpectedOutputField)
        }
        if processTest2 {
            print("queuing test2")
            processingQueue.addOperation(runSchemeOpTest2)
        } else {
            resetTestUI(test2StatusLabel, inputField: test2InputField, outputField: test2ExpectedOutputField)
        }
        if processTest3 {
            print("queuing test3")
            processingQueue.addOperation(runSchemeOpTest3)
        } else {
            resetTestUI(test3StatusLabel, inputField: test3InputField, outputField: test3ExpectedOutputField)
        }
        if processTest4 {
            print("queuing test4")
            processingQueue.addOperation(runSchemeOpTest4)
        } else {
            resetTestUI(test4StatusLabel, inputField: test4InputField, outputField: test4ExpectedOutputField)
        }
        if processTest5 {
            print("queuing test5")
            processingQueue.addOperation(runSchemeOpTest5)
        } else {
            resetTestUI(test5StatusLabel, inputField: test5InputField, outputField: test5ExpectedOutputField)
        }
        if processTest6 {
            print("queuing test6")
            processingQueue.addOperation(runSchemeOpTest6)
        } else {
            resetTestUI(test6StatusLabel, inputField: test6InputField, outputField: test6ExpectedOutputField)
        }
    }
}

