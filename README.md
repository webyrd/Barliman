# Barliman

Prototype smart text editor

---

"He thinks less than he talks, and slower; yet he can see through a brick wall in time (as they say in Bree)."

--Gandalf the Grey, about Barliman Butterbur

The Lord of the Rings: The Fellowship of the Ring

J. R. R. Tolkien

---

This editor uses miniKanren (http://minikanren.org/), and a relational Scheme interpreter written in miniKanren (Byrd, Holk, and Friedman, 2012, http://dl.acm.org/citation.cfm?id=2661105 or http://webyrd.net/quines/quines.pdf), to provide real-time feedback to the code editor using program synthesis.

Chez Scheme in turn uses the miniKanren implementation and relational interpreter implementation contained in the `mk-and-rel-interp` directory.


---------------------------------------

![append example 1](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append1.jpg "append example 1")

---------------------------------------

The cocoa version of the editor is written in Swift, and has been tested under OS X 10.11.4 and XCode 7.3.1.  Eventually the editor will be crossplatform.  I'm starting with cocoa since I'm developing on a Mac, and I want to make sure I don't box myself into a corner with the user interface/performance as I experiment with the design and the interface.  The cocoa version of Barliman calls out to Chez Scheme (https://github.com/cisco/ChezScheme), which must be installed separately, and which is assumed to reside in `/usr/local/bin/scheme`.

IMPORTANT:  The cocoa version of Barliman does its best to clean up the Scheme processes it launches.  However, it is wise to run `top -o cpu` from the terminal after playing with Barliman, to make sure errant Scheme processes aren't running in the background.  If these tasks are running, you can kill them using `kill -9 <pid>`, where `<pid>` is the process identifier listed from the `top` command.

To use the cocoa version of Barliman, first build and launch the application from XCode.  The application currently has a single window, with a single editable pane.  Enter Scheme expressions that run in the relational Scheme interpreter, such as:

```
((lambda (x) x) 5)
```

Each time the text changes, a Chez Scheme process will be launched, attempting to evaluate the expression in the relational Scheme interpreter.  If the expression is not a valid Scheme s-expression, Chez will complain, an Baliman will display the code in red.  If the expression is a legal s-expression, and the corresponding miniKanren query to the relational interpeter succeeds, the value of the query will be displayed below the editable text box.  If the query fails, the empty list will be displayed in the text box.

For more interesting answers, you can use the logic variables A through G, upper-case.  Make sure to unquote the logic variables:

```
((lambda (x) ,A) ,B)
```

---------------------------------------


Thanks to Michael Ballantyne, Kenichi Asai, Alan Borning, Nada Amin, Guannan Wei, Pierce Darragh, Alex Warth, Michael Adams, Tim Johnson, Evan Czaplicki, Stephanie Weirich, Nehal Patel, Andrea Magnorsky, Reid McKenzie, Emina Torlak, Chas Emerick, Martin Clausen, Devon Zuegel, Daniel Selifonov, Greg Rosenblatt, Michael Nielsen, David Kahn, Brian Mastenbrook, Orchid Hybrid, Rob Zinkov, Margaret Staples, Matt Hammer, Dan Friedman, Ron Garcia, Rich Hickey, Phil Wadler, Matt Might, participants of my 2016 PEPM tutorial on miniKanren, and particants of the 'As We May Thunk' group (http://webyrd.net/thunk.html), for suggestions, encouragement, and inspiration.

The definition of `letrec` in the main interpreter is based based on Dan Friedman's code, using the "half-closure" approach from Reynold's definitional interpreters.


Barliman is intended to be an improved version of the very crude 'miniKanren playground' I showed at my 2016 PEPM tutorial on miniKanren: https://github.com/webyrd/minikanren-playground



TODO:

* add STLC as an example, complete with type inferencer
* cancel the allTests operation if any single test fails, since in that case allTests cannot possibly succeed
* wait part of a second to see if there are more keystrokes before launching Scheme processes.  Sort of like XCode (I assume XCode is doing this).  Would be more resource friendly, less distracting, and would make typing quickly more responsive.  Could probably do this using an timer.
* perhaps be able to drag and drop subexpressions from the best guess pane onto variables in the definition pane.  And also be able to replace an extort subexpression in the definition pane with a logic variable.
* think about contextual menus/right click and also drag and shift-drag.  What should these do?
* make sure Semantics and the main Barliman windows can be reopened if the user closes them!  Currently there doesn't seem to be a way to get the window back.  Perhaps allow the user to hide the windows, but not close them?  What is the preferred Mac way?
* add paren hilighting/blinking when the parens match
* for the case in which a simple function is being used to generate test inputs and answers for a more complex version of the same function, may need or want a grounder to make sure answers are fully ground.  May also want a grounder for code, esp for the best guess pane.  Although grounding code may not be necessary or ideal.
* would be smart to only re-run Scheme processes when the Scheme code actually *changes* -- for example, white space characters outside of an S-expr shouldn't trigger re-evaluation.  One way would be to compare "before" and "after" S-exprs to see if anything has changed.  Could run a single Scheme instance and call `equal?` to see if the code has actually changed.  This could be a big win for expensive computations.
* support multiple definitions and mutual recursion
* show reified test inputs and outputs upon success, for all tests
* add ability to save and load examples/tests/semantics, and include interesting examples, such as a tiny Scheme interpreter written in Scheme, state machine using mutual recursion, examples from pearls, etc.
* add structured editor for semantics and for type inferencer (as an alternative to/in addition to the free-form editor)
* add 'transpose S-expression' and other useful Emacs editing commands
* move as much work as possible into NSTasks, such as loading files.
* possibly add pairs of tests as processes, once individual tests complete successfully
* add ability to change the evaluator rules and perhaps an explicit grammar as well
* change green text to bold font instead
* add structured editing capability, with automatic addition of right parens, auto-addition of logic variables, and perhaps something like paredit
* add syntax-directed auto-indentation of code 
* figure out how to do syntax-directed hilighlighting, and precise hilighting of syntax errors.  May not be as important if I go the structured editor route.  Although perhaps this should be an option, either way.
* remove possible race condition with respect to 'barliman-query.scm' temp file
* add documentation/tutorial
* add paper prototype for desired features
* move 'barliman-query.scm' temporary file to a more suitable location than 'Documents' directory, or get rid of the temp file entirely
* experiment with store passing style and small step interpreters
* get rid of hardcoded path to Chez executable
* add input/output examples
* find a cleaner and more flexible way to construct the program sent to Chez
* add ability to save and load programs
* add "accept suggested completion" button
* would be smarter/less resource intense to not launch all the tests again when the text in a single test changes.  Only that test and allTests need be re-run, in theory.  Getting the UI to display the state of everything properly may be a little subtle, though.
* differential relational interpreters

LONGER TERM:

* automatic test generation/fuzzing
* add arithmetic to the main interpreter 
* make the editor cross-platform; Clojure/Clojurescript/core.logic and JavaScript versions could be especially nice
* explore incremental computing with the editor
* add type inferencer
* test generation of typed test programs
* partial evaluation of the interpreter to speed up evaluation
* add support for macros
* explore predicates/generators/QuickCheck-like functionality
* add ability to fill in test input/outputs, given a fully or mostly specified definition

POSSIBLE USE CASES:

* write simple implementtation of a function, generate test from that function, then use those tests to guide the more sophisticated implementation.  Or more generally, continually test the partially-implemented function vs the fully implemented but perhaps less efficient function.

SUSPECT IDEAS:

* could just call out to Scheme one the program becomes grounded.  However, the semantics and even the grammar may not match that of the interpreter used by miniKanren, so this seems difficult or impossible to do properly.

KNOWN LIMITATIONS AND BUGS:

* the system currently uses the evaluator/`evalo` to determine if a program is legal.  This means that syntactically illegal code will not be caught, provided that the code is not actually evaluated.  Should probably add separate grammar, and clearly display grammar errors separately from semantic errors.
* the main interpreter's `letrec` form supports only a single lambda, which precludes writing mutually-recursive functions
* the `begin` form allows for only one definition
* the `lambda` form does not contain an implicit `begin`
* closing one of the windows means the window cannot be reopened!  oops
