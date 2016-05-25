//
//  EditorWindowController.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

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
        let query_file = "barliman-query.scm"
        
        let mk_vicare_path_string = mk_vicare_path as! String
        let mk_path_string = mk_path as! String
        let interp_path_string = interp_path as! String

        let load_mk_vicare_string: String = "(load \"\( mk_vicare_path_string )\")"
        let load_mk_string: String = "(load \"\( mk_path_string )\")"
        let load_interp_string: String = "(load \"\( interp_path_string )\")"

        let text: String = load_mk_vicare_string +
                           load_mk_string +
                           load_interp_string +
            "(write " + "(run 1 (q) (fresh (A B C D E F G) (evalo `" +
            editableSchemeField.stringValue +
            " q))) )"
        
        print("text = \n\( text )\n")
        
        var path: NSURL
        path = NSURL()
        
        // write the temporary file containing the query to the user's Document directory.  This seems a bit naughty.  Where is the right place to put this?  In ~/.barliman, perhaps?
        if let dir = NSSearchPathForDirectoriesInDomains(NSSearchPathDirectory.DocumentDirectory, NSSearchPathDomainMask.AllDomainsMask, true).first {
            path = NSURL(fileURLWithPath: dir).URLByAppendingPathComponent(query_file)
            
            // write the query file
            do {
                try text.writeToURL(path, atomically: false, encoding: NSUTF8StringEncoding)
            }
            catch {
                // this error handling could be better!  :)
                print("couldn't write to query file")
            }
        }
        
        
        // path to the Scheme file containing the miniKanren query
        var schemeScriptPathString: String = ""
        schemeScriptPathString = path.path!
        
        // create the operation that will be placed in the operation queue
        let runSchemeOp: RunSchemeOperation = RunSchemeOperation.init(editorWindowController: self, schemeScriptPathString: schemeScriptPathString)
        
        // wait until the previous operations kill their tasks and finish, before adding the new operation
        processingQueue.waitUntilAllOperationsAreFinished()
        
        // now that the previous operations have completed, safe to add the new operation
        processingQueue.addOperation(runSchemeOp)
    }

    
}
