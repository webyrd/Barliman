//
//  SchemeEngine.swift
//  Barliman
//
//  Copyright © 2026 William E. Byrd.
//  Released under MIT License (see LICENSE file)

import Foundation

/// Manages query generation, debouncing, and Chez Scheme process orchestration.
/// Reads user input from `BarlimanModel` and writes results back to it.
class SchemeEngine {
    private let model: BarlimanModel
    private let processingQueue = OperationQueue()
    private var debounceWorkItem: DispatchWorkItem?
    private var interpreterCode: String?
    private weak var allTestsOperation: SchemeProcessRunner?

    init(model: BarlimanModel) {
        self.model = model
        loadInterpreterCode("interp")
    }

    // MARK: - Debounce

    /// Called whenever the user edits any text field. Debounces by 1 second before running queries.
    func onTextChanged() {
        debounceWorkItem?.cancel()
        let workItem = DispatchWorkItem { [weak self] in
            // Capture model state on main thread, then run queries on background
            self?.prepareAndRunQueries()
        }
        debounceWorkItem = workItem
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0, execute: workItem)
    }

    // MARK: - Process Control

    func cancelAllTestsOperation() {
        allTestsOperation?.cancel()
    }

    func cancelAllOperations() {
        processingQueue.cancelAllOperations()
    }

    func cleanup() {
        debounceWorkItem?.cancel()
        processingQueue.cancelAllOperations()
        processingQueue.waitUntilAllOperationsAreFinished()
    }

    // MARK: - Interpreter Loading

    private func loadInterpreterCode(_ interpFileName: String) {
        let bundle = Bundle.main
        guard let interpPath = bundle.path(forResource: interpFileName, ofType: "scm", inDirectory: "mk-and-rel-interp") else {
            print("Error: Could not find interpreter file '\(interpFileName).scm' in bundle.")
            return
        }
        do {
            interpreterCode = try String(contentsOf: URL(fileURLWithPath: interpPath), encoding: .utf8)
        } catch {
            print("Error: Can't load interpreter: \(error)")
        }
    }

    private func getInterpreterCode() -> String {
        guard let code = interpreterCode else {
            fatalError("Interpreter code was not loaded. Ensure loadInterpreterCode() was called.")
        }
        return code
    }

    // MARK: - Query Generation

    private func makeQuerySimpleForMondoSchemeFileString(_ interpString: String,
                                                         mkVicarePath: String,
                                                         mkPath: String,
                                                         definitionText: String) -> String {
        let loadMkVicareString = "(load \"\(mkVicarePath)\")"
        let loadMkString = "(load \"\(mkPath)\")"

        let querySimple = makeQueryString(definitionText,
                                          body: ",_",
                                          expectedOut: "q",
                                          simple: true,
                                          name: "-simple")

        return loadMkVicareString + "\n" +
               loadMkString + "\n" +
               interpString + "\n" +
               querySimple
    }

    private func makeAllTestsQueryString(definitionText: String,
                                           testInputs: [String],
                                           testOutputs: [String],
                                           processTest: [Bool]) -> String {
        let allTestInputs = (0..<6).map { i in (processTest[i] ? testInputs[i] : "") + " " }.joined()
        let allTestOutputs = (0..<6).map { i in (processTest[i] ? testOutputs[i] : "") + " " }.joined()
        let bundle = Bundle.main

        func loadBundleFile(_ name: String, type: String) -> String {
            guard let path = bundle.path(forResource: name, ofType: type, inDirectory: "mk-and-rel-interp") else {
                print("!!!!!  LOAD_ERROR -- can't find \(name).\(type)\n")
                return ""
            }
            do { return try String(contentsOfFile: path) }
            catch {
                print("!!!!!  LOAD_ERROR -- can't load \(name).\(type)\n")
                return ""
            }
        }

        let alltestsPart1 = loadBundleFile("interp-alltests-query-string-part-1", type: "swift")
        let alltestsPart2 = loadBundleFile("interp-alltests-query-string-part-2", type: "swift")

        let evalFlagsFast = "(allow-incomplete-search)"
        let evalFlagsComplete = "(disallow-incomplete-search)"
        let evalStringFast = "(begin \(evalFlagsFast) (results))"
        let evalStringComplete = "(begin \(evalFlagsComplete) (results))"

        let allTestWriteString = "(define (ans-allTests)\n" +
            "  (define (results)\n" +
            alltestsPart1 + "\n" +
            "        (== `( \(definitionText) ) defn-list)" + "\n\n" +
            alltestsPart2 + "\n" +
            "(== `(" +
            definitionText +
            ") defns) (appendo defns `(((lambda x x) " +
            allTestInputs +
            ")) begin-body) (evalo `(begin . ,begin-body) (list " +
            allTestOutputs +
            ")" +
            ")))))\n" +
            "(let ((results-fast \(evalStringFast)))\n" +
            "  (if (null? results-fast)\n" +
            "    \(evalStringComplete)\n" +
            "    results-fast)))"

        let fullString = ";; allTests\n" + allTestWriteString
        return fullString
    }

    private func makeQueryString(_ defns: String,
                                  body: String,
                                  expectedOut: String,
                                  simple: Bool,
                                  name: String) -> String {

        let parseAnsString = "(define (parse-ans\(name)) (run 1 (q)" + "\n" +
            " (let ((g1 (gensym \"g1\")) (g2 (gensym \"g2\")) (g3 (gensym \"g3\")) (g4 (gensym \"g4\")) (g5 (gensym \"g5\")) (g6 (gensym \"g6\")) (g7 (gensym \"g7\")) (g8 (gensym \"g8\")) (g9 (gensym \"g9\")) (g10 (gensym \"g10\")) (g11 (gensym \"g11\")) (g12 (gensym \"g12\")) (g13 (gensym \"g13\")) (g14 (gensym \"g14\")) (g15 (gensym \"g15\")) (g16 (gensym \"g16\")) (g17 (gensym \"g17\")) (g18 (gensym \"g18\")) (g19 (gensym \"g19\")) (g20 (gensym \"g20\")))" + "\n" +
            "(fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _) (parseo `(begin \(defns) \(body)))))))"

        let parseWithFakeDefnsAnsString = "(define (parse-ans\(name)) (run 1 (q)" + "\n" +
            " (let ((g1 (gensym \"g1\")) (g2 (gensym \"g2\")) (g3 (gensym \"g3\")) (g4 (gensym \"g4\")) (g5 (gensym \"g5\")) (g6 (gensym \"g6\")) (g7 (gensym \"g7\")) (g8 (gensym \"g8\")) (g9 (gensym \"g9\")) (g10 (gensym \"g10\")) (g11 (gensym \"g11\")) (g12 (gensym \"g12\")) (g13 (gensym \"g13\")) (g14 (gensym \"g14\")) (g15 (gensym \"g15\")) (g16 (gensym \"g16\")) (g17 (gensym \"g17\")) (g18 (gensym \"g18\")) (g19 (gensym \"g19\")) (g20 (gensym \"g20\")))" + "\n" +
            " (fresh (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z _) (fresh (names dummy-expr) (extract-nameso `( \(defns) ) names) (parseo `((lambda ,names \(body)) ,dummy-expr)))))))"

        let bundle = Bundle.main

        func loadBundleQueryFile(_ name: String) -> String {
            guard let path = bundle.path(forResource: name, ofType: "swift", inDirectory: "mk-and-rel-interp") else {
                print("!!!!!  LOAD_ERROR -- can't find \(name)\n")
                return ""
            }
            do { return try String(contentsOfFile: path) }
            catch {
                print("!!!!!  LOAD_ERROR -- can't load \(name)\n")
                return ""
            }
        }

        let evalPart1 = loadBundleQueryFile("interp-eval-query-string-part-1")
        let evalPart2 = loadBundleQueryFile("interp-eval-query-string-part-2")

        let evalString = evalPart1 + "\n" +
            "        (== `( \(defns) ) defn-list)" + "\n" +
            evalPart2 + "\n" +
            " (evalo `(begin \(defns) \(body)) \(expectedOut)))))"

        let evalFlagsFast = "(allow-incomplete-search)"
        let evalFlagsComplete = "(disallow-incomplete-search)"
        let evalStringFast = "(begin \(evalFlagsFast) \(evalString))"
        let evalStringComplete = "(begin \(evalFlagsComplete) \(evalString))"
        let evalStringBoth = "(let ((results-fast \(evalStringFast)))\n" +
                             "  (if (null? results-fast)\n" +
                             "    \(evalStringComplete)\n" +
                             "     results-fast))"

        let defineAnsString = "(define (query-val\(name))" + "\n" +
                              "  (if (null? (parse-ans\(name)))" + "\n" +
                              "      'parse-error" + "\n" +
                              "      \(evalStringBoth)))"

        let fullString = (simple ? ";; simple query" : ";; individual test query") + "\n\n" +
                          (simple ? parseAnsString : parseWithFakeDefnsAnsString) + "\n\n" +
                          defineAnsString + "\n\n"

        return fullString
    }

    // MARK: - Run Queries

    /// Captures model state on the main thread, then dispatches query execution to background.
    private func prepareAndRunQueries() {
        // Read model state on main thread
        let tests = model.tests
        let processTest = tests.map { $0.isActive }
        let definitionText = model.definitionText
        let testInputs = tests.map { $0.input }
        let testOutputs = tests.map { $0.expectedOutput }
        let testIds = tests.map { $0.id }

        // Set thinking status immediately on main thread
        processingQueue.cancelAllOperations()
        model.definitionStatus = .thinking
        model.bestGuessStatus = .thinking
        for i in 0..<6 where processTest[i] {
            model.tests[i].status = .thinking
        }
        for i in 0..<6 where !processTest[i] {
            model.tests[i].status = .idle
        }

        // Run the rest on a background queue to avoid blocking main
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            self?.runQueries(definitionText: definitionText,
                             testInputs: testInputs,
                             testOutputs: testOutputs,
                             testIds: testIds,
                             processTest: processTest)
        }
    }

    private func runQueries(definitionText: String,
                            testInputs: [String],
                            testOutputs: [String],
                            testIds: [Int],
                            processTest: [Bool]) {

        let bundle = Bundle.main
        guard let mkVicarePath = bundle.path(forResource: "mk-vicare", ofType: "scm", inDirectory: "mk-and-rel-interp/mk"),
              let mkPath = bundle.path(forResource: "mk", ofType: "scm", inDirectory: "mk-and-rel-interp/mk") else {
            print("Error: Could not find miniKanren files in bundle.")
            return
        }

        let interpString = getInterpreterCode()

        let querySimpleContents = makeQuerySimpleForMondoSchemeFileString(interpString,
                                                                         mkVicarePath: mkVicarePath,
                                                                         mkPath: mkPath,
                                                                         definitionText: definitionText)

        let testQueryStrings = (0..<6).map { i in
            makeQueryString(definitionText,
                            body: testInputs[i],
                            expectedOut: testOutputs[i],
                            simple: false,
                            name: "-test\(testIds[i])")
        }

        let allTestsQueryString = makeAllTestsQueryString(definitionText: definitionText,
                                                             testInputs: testInputs,
                                                             testOutputs: testOutputs,
                                                             processTest: processTest)

        // Load templates
        func loadTemplate(_ name: String) -> String {
            guard let path = bundle.path(forResource: name, ofType: "swift", inDirectory: "mk-and-rel-interp") else {
                print("!!!!!  LOAD_ERROR -- can't find template \(name)\n")
                return ""
            }
            do { return try String(contentsOfFile: path) }
            catch {
                print("!!!!!  LOAD_ERROR -- can't load \(name)\n")
                return ""
            }
        }

        let simpleTemplate = loadTemplate("barliman-new-simple-query-template")
        let testTemplate = loadTemplate("barliman-new-test-query-template")
        let allTestsTemplate = loadTemplate("barliman-new-alltests-query-template")

        // Write query files to tmp
        let tmpDir = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)

        let simpleForMondoPath = tmpDir.appendingPathComponent("barliman-query-simple-for-mondo-scheme-file.scm")
        let testQueryPaths = (1...6).map { tmpDir.appendingPathComponent("barliman-new-query-test\($0).scm") }
        let testActualQueryPaths = (1...6).map { tmpDir.appendingPathComponent("barliman-new-query-actual-test\($0).scm") }
        let simplePath = tmpDir.appendingPathComponent("barliman-new-query-simple.scm")
        let allTestsPath = tmpDir.appendingPathComponent("barliman-new-query-alltests.scm")
        let actualAllTestsPath = tmpDir.appendingPathComponent("barliman-new-query-actual-alltests.scm")

        let loadFileString = "(define simple-query-for-mondo-file-path \"\(simpleForMondoPath.path)\")"

        let simpleQueryString = loadFileString + "\n\n" + simpleTemplate

        let allTestsFullString = loadFileString + "\n\n" +
            "(define actual-query-file-path \"\(actualAllTestsPath.path)\")" + "\n\n" +
            allTestsTemplate

        let testFullStrings = (0..<6).map { i -> String in
            loadFileString + "\n\n" +
            "(define actual-query-file-path \"\(testActualQueryPaths[i].path)\")" + "\n\n" +
            "(define (test-query-fn) (query-val-test\(i + 1)))" + "\n\n\n" +
            testTemplate
        }

        do {
            try querySimpleContents.write(to: simpleForMondoPath, atomically: false, encoding: .utf8)
            try simpleQueryString.write(to: simplePath, atomically: false, encoding: .utf8)
            try allTestsFullString.write(to: allTestsPath, atomically: false, encoding: .utf8)
            try allTestsQueryString.write(to: actualAllTestsPath, atomically: false, encoding: .utf8)

            for i in 0..<6 {
                try testFullStrings[i].write(to: testQueryPaths[i], atomically: false, encoding: .utf8)
                try testQueryStrings[i].write(to: testActualQueryPaths[i], atomically: false, encoding: .utf8)
            }
        } catch {
            print("couldn't write to query files: \(error)")
        }

        // Create operations
        let simpleOp = SchemeProcessRunner(model: model, engine: self, scriptPath: simplePath.path, taskType: .simple)
        let allTestsOp = SchemeProcessRunner(model: model, engine: self, scriptPath: allTestsPath.path, taskType: .allTests)
        allTestsOperation = allTestsOp

        let testTaskTypes: [TaskType] = [.test1, .test2, .test3, .test4, .test5, .test6]
        let testOps = (0..<6).map { i in
            SchemeProcessRunner(model: model, engine: self, scriptPath: testQueryPaths[i].path, taskType: testTaskTypes[i])
        }

        // Wait for previous operations to finish
        processingQueue.waitUntilAllOperationsAreFinished()

        // Add new operations
        processingQueue.addOperation(allTestsOp)
        processingQueue.addOperation(simpleOp)

        for i in 0..<6 {
            if processTest[i] {
                print("queuing test\(i + 1)")
                processingQueue.addOperation(testOps[i])
            }
        }
    }
}
