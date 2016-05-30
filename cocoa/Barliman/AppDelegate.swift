//
//  AppDelegate.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    var editorWindowController: EditorWindowController?
    var semanticsWindowController: SemanticsWindowController?
    
    func applicationDidFinishLaunching(aNotification: NSNotification) {
        
        // Create window controllers with XIB files of the same name
        let editorWindowController = EditorWindowController()
        let semanticsWindowController = SemanticsWindowController()

        // Put the windows of the controllers on screen
        editorWindowController.showWindow(self)
        semanticsWindowController.showWindow(self)
        
        // Set the property to point to the window controllers
        self.editorWindowController = editorWindowController
        self.semanticsWindowController = semanticsWindowController
    }

    func applicationWillTerminate(aNotification: NSNotification) {
        // Insert code here to tear down your application
        editorWindowController!.cleanup()
        semanticsWindowController!.cleanup()
    }
    
}
