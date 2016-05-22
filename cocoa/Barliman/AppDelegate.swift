//
//  AppDelegate.swift
//  Barliman
//
//  Created by William Byrd on 5/14/16.
//  Copyright © 2016 William E. Byrd. All rights reserved.
//

import Cocoa

@NSApplicationMain
class AppDelegate: NSObject, NSApplicationDelegate {

    var editorWindowController: EditorWindowController?
    
    func applicationDidFinishLaunching(aNotification: NSNotification) {
        
        // Create a window controller with a XIB file of the same name
        let editorWindowController = EditorWindowController()
        
        // Put the window of the controller on screen
        editorWindowController.showWindow(self)
        
        // Set the property to point to the window controller
        self.editorWindowController = editorWindowController
    }

    func applicationWillTerminate(aNotification: NSNotification) {
        // Insert code here to tear down your application
    }
    
}
