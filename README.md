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



The cocoa version of the editor is written in Swift, and has been tested under OS X 10.11.4 and XCode 7.3.1.  Eventually the editor will be crossplatform.  I'm starting with cocoa since I'm developing on a Mac, and I want to make sure I don't box myself into a corner with the user interface/performance as I experiment with the design and the interface.  The cocoa version of Barliman calls out to Chez Scheme (https://github.com/cisco/ChezScheme), which must be installed separately, and which is assumed to reside in `/usr/local/bin/scheme`.

IMPORTANT:  The cocoa version of Barliman does its best to clean up the Scheme processes it launches.  However, it is wise to run 'top -o cpu' from the terminal after playing with Barliman, to make sure errant Scheme processes aren't running in the background.  If these tasks are running, you can kill them using 'kill -9 <pid>', where <pid> is the process identifier listed from the 'top' command.




Thanks to Michael Ballantyne, Kenichi Asai, Alan Borning, Nada Amin, Guannan Wei, Pierce Darragh, Alex Warth, Michael Adams, Tim Johnson, Evan Czaplicki, Stephanie Weirich, Nehal Patel, Andrea Magnorsky, Reid McKenzie, Emina Torlak, Chas Emerick, Martin Clausen, Devon Zuegel, Daniel Selifonov, Greg Rosenblatt, Michael Nielsen, David Kahn, Brian Mastenbrook, Orchid Hybrid, Rob Zinkov, Margaret Staples, Matt Hammer, Dan Friedman, Ron Garcia, Rich Hickey, Matt Might, participants of my 2016 PEPM tutorial on miniKanren, and particants of the 'As We May Thunk' group (http://webyrd.net/thunk.html), for suggestions, encouragement, and inspiration.

Barliman is intended to be an improved version of the very crude 'miniKanren playground' I showed at my 2016 PEPM tutorial on miniKanren: https://github.com/webyrd/minikanren-playground



TODO:

* remove possible race condition with respect to 'barliman-query.scm' temp file
* add documentation/tutorial
* add paper prototype for desired features
* move 'barliman-query.scm' temporary file to a more suitable location than 'Documents' directory, or get rid of the temp file entirely
* get rid of hardcoded path to Chez executable
* add input/output examples
* find a cleaner and more flexible way to construct the program sent to Chez
* create process tree to try the examples
* add ability to change the evaluator rules and perhaps an explicit grammar as well
* add ability to save and load programs
* add "accept suggested completion" button


LONGER TERM:

* make the editor cross-platform; Clojure/Clojurescript/core.logic and JavaScript versions could be especially nice
* add structured editing capability, with auto-addition of logic variables
* explore incremental computing with the editor
* add type inferencer
