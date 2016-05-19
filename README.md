# Barliman

Prototype smart text editor

---

"He thinks less than he talks, and slower; yet he can see through a brick wall in time as they say in Bree"

--Gandolf the Grey, about Barliman Butterbur

The Lord of the Rings: The Fellowship of the Ring

J. R. R. Tolkien

---

This editor uses miniKanren (http://minikanren.org/), and a relational Scheme interpreter written in miniKanren (Byrd, Holk, and Friedman, 2012, http://dl.acm.org/citation.cfm?id=2661105 or http://webyrd.net/quines/quines.pdf), to provide real-time feedback to the code editor using program synthesis.

The editor is written in Swift, and has been tested under OS X 10.11.4 and XCode 7.3.1.

The editor calls out to Chez Scheme (https://github.com/cisco/ChezScheme), which must be installed separately, and which is assumed to reside in `/usr/local/bin/scheme`.

Chez Scheme in turn uses the miniKanren implementation and relational interpreter implementation contained in the `mk-and-rel-interp` directory.



TODO:

* add documentation/tutorial
* get rid of the hardcoded paths to Will's lappy
* kill all running Chez processes spawned by the editor each time the program changes (not just after each keystroke!).
* add input/output examples
* create process tree to try the examples
* add ability to change the evaluator rules and perhaps an explicit grammar as well
* add ability to save and load programs
* add "accept suggested completion" button


LONGER TERM

* add structured editing capability, with auto-addition of logic variables
* explore incremental computing with the editor
* add type inferencer
