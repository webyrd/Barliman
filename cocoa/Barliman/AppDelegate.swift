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

    func applicationDidFinishLaunching(_ aNotification: Notification) {
        
        // Create window controllers with XIB files of the same name
        let editorWindowController = EditorWindowController()
        
        // Put the windows of the controllers on screen
        editorWindowController.showWindow(self)
        
        // Set the property to point to the window controllers
        self.editorWindowController = editorWindowController
    }

    func applicationWillTerminate(_ aNotification: Notification) {
        // Insert code here to tear down your application
        editorWindowController!.cleanup()
    }
    
}

