# Barliman

---------------------------------------

"He thinks less than he talks, and slower; yet he can see through a brick wall in time (as they say in Bree)."

--Gandalf the Grey, about Barliman Butterbur

The Lord of the Rings: The Fellowship of the Ring

J. R. R. Tolkien

---------------------------------------

## Barliman overview

Barliman is a prototype "smart editor" that performs real-time program synthesis to try to make the programmer's life a little easier.  Barliman has several unusual features:

* given a set of tests for some function `foo`, Barliman tries to "guess" how to fill in an partially-specified definition of `foo` to make all of the tests pass;

* given a set of tests for some function `foo`, Barliman tries to prove that a partially-specified definition of `foo` is inconsistent with one or more of the tests;

* given a fully or mostly-specified definition of some function `foo`, Barliman will attempt to prove that a partially-specified test is consistent with, or inconsistent with, the definition of `foo`.

Barliman is general enough to handle multiple programming languages.  In fact, the user can even specify their own programming language, or change the semantics or syntax of one of the default languages that come with Barliman.  The default language for Barliman is a small but Turing-complete subset of side-effect-free Scheme that supports recursion, list operations, higher-order functions, multi-argument and variadic functions, and a few other features.

### Purpose of Barliman

(test my hypothesis that even very modest or slow program synthesis can be useful, if it is part of an interactive conversation with a programmer)

(explore utility of interactive synthesis)

(explore design for such an interactive tool)

(try to inspire others to build similar tools, perhaps using radically different implementation techniques)


### Barliman in action

Here are a few screenshots of Barliman, using the Mac implementation as of June 4, 2016.

The first screenshot shows the main editor window.  The `Scheme Definition` edit pane contains the complete (fully instantiated) and correct definition of `append`, the list concatenation function in Barliman's default "miniScheme" language.  `append` will be our simple running example in these screenshots.  The edit window also contains three tests; each test contains an input expression, and the expected value of that expression.  The `Best Guess` pane, which is not editable by the user, contains the same fully instantiated definition of `append` as in the `Scheme Definition` edit pane.

All the text in the editor window is black, indicating that all the information in the editor is consistent and valid.  The definition of `append` is a valid symbolic expression (s-expression), and is indeed a syntactically valid miniScheme definition.  The test expressions and expected values are syntactically valid, and consistent with each other.

The editor window displayed in this first screeenshot is similar in spirit to a modern integrated development environment (IDE) that runs tests whenever the code to be tested is modified.

Let's see how we might have gotten to the final version of `append` using Barliman.

#### screenshot 1: Fully instantiated definition of append

![append example 1 -- fully instantiated code](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append01.jpg "append example 1 -- fully instantiated code")



Screenshot 2 shows the empty main editor window, immediatly after starting Barliman.  We know we want to define `append`, so in true test-drived development style we begin by writing our tests cases.


#### screenshot 2: 

![append example 2 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append13.jpg "append example 2 -- ")



Screenshot 3 shows the main editor window with our three tests.  The first test says that if we append the empty list to the empty list, we should get back the empty list.  You should be able to figure out the other two tests.

The text for all three tests are red, indicating that none of the tests pass.  This isn't surprising, perhaps, since we haven't started to define the `append` function.

(From an interface design standpoint, whether to use colors, which colors to use, which text fields to hilight, etc., are all open questions in my mind.  Over time I hope to make it much more clear exactly which part of the code is failing, and why.)

#### screenshot 3: 

![append example 3 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append09.jpg "append example 3 -- ")



Screenshot 4 shows the main editor window after we have begun defining `append` in the `Scheme Definition` edit pane.  Our parentheses are not balanced -- we haven't yet typed a closing parenthesis for the `define` form.  Because of the missing parenthesis, the definition is not a legal Scheme s-expression.  The tests cannot pass, of course, since `append` isn't even an s-expression.  Barliman recognizes this, and turns the text in the `Scheme Definition` edit pane, and the text in the test edit fields, a sickly green color.

(Future versions of Barliman should include a structured editor that will automatically insert balanced parentheses.)

#### screenshot 4: 

![append example 4 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append11.jpg "append example 4 -- ")



Screenshot 5 shows the main editor window after we have added the closing parenthesis in our partial definition of `append` in the `Scheme Definition` edit pane.  The partial definition of `append` is now a legal s-expression.  However, the definition of `append` is not syntactically valid according to the rules of miniScheme.  Of course, this invalid definition of `append` causes all the tests to fail as well.  Barliman recognizes this, and turns the text in the `Scheme Definition` edit pane, and the text in the test edit fields, red.

(Currently Barliman doesn't actually check that definitions are grammatically correct.  Rather, Barliman uses evaluation of the tests to check whether code is *semantically* legal, rather than syntactically legal.  Future versions of Barliman will probably include explicit grammars that are checked, in addition to semantic rules.)

#### screenshot 5: 

![append example 5 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append10.jpg "append example 5 -- ")


In screenshot 6 we can see that the programmer has partially specified the defintion of `append`.  The definition is a syntactally-correct s-expression, and indeed is a syntactically correct use of miniScheme's `define` form.  Importantly, the definition of `append` is only partially specified, and contains four (logic) variables (`A`, `B`, `C`, and `D`) representing unknown subexpressions.  In Barliman variables representing unknown subexpressions are single-letter upper-case variables `A` through `Z`.  (Note to Schemers: The comma (`,`) that usually occurs before these letters is necessary because the code in the `Scheme Definition` edit pane is implicitly quasiqoted.)

Given the partially-specified defintion of `append` in the `Scheme Definition` edit pane, along with the three tests, Barliman is able to correctly "guess" the code corresponding to these variables.  The correct and complete definition of `append` is displayed in the `Best Guess` pane.  Barliman guesses the correct code in this case in a second or less.  All of the text in the main editor window is black, indicating that all of the code is syntactically correct, and that all three tests pass given the completed definition of `append` shown in the `Best Guess` pane. 

#### screenshot 6: 

![append example 6 -- partially instantiated code filled in](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append07.jpg "append example 6 -- partially instantiated code filled in")



Screenshot 7 shows an *incorrect* partial definition of `append`.  As in the previous screenshot, the partial definition of `append` contains variables representing unknown subexpressions (the `,A` and `,B` and `,C`).  However, in this case the first argument to `cons` is incorrect.  The first argument to `cons` should be `(car l)`, as shown in screenshot 1.  Alternatively, the first argument to `cons` could be an incomplete expression containing a variable representating an unknown subexpression, such as `(car ,B)` from screenshot 6.  Here, however, the first argument to `cons` is the expression `(cdr l)`.  The red text for tests 2 and 3 indicate that these tests are *incompatible* with the partial definition of `append` in the `Scheme Definition` edit pane.  That is, there are no legal miniScheme expressions that could be substituted for the variables `A`, `B`, and `C` that would make tests 2 and 3 pass.

#### screenshot 7:

![append example 7 -- partially instantiated code incompatible with tests](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append08.jpg "append example 7 -- partially instantiated code incompatible with tests")




#### screenshot 8: 

![append example 8 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append/append12.jpg "append example 8 -- ")



---------------------------------------

## Advantages and limitations of Barliman

### Advantages of Barliman

Barliman is *flexible*.  Barliman can handle operational semantics for various programming languges.  Users can add their own semantics, or modify the semantics for languages that are included with Barliman.  Barliman does not require the language be statically typed, or that the user has supplied enough tests to fully synthesize the function being defined. 

Barliman is *interactive*.  Any change to the definition of a function, the corresponding tests, or even the semantics immediately re-triggers the program synthesis solver.

### Limitations of Barliman

Barliman can be extremely slow when it comes to program synthesis, and can easily get "stuck", possibly taking hours and tens of gigabytes of RAM to synthesize small fragments of code.  Since the default "miniScheme" language is dynamically typed, Barliman cannot take advantage of types to limit the space of programs to be considered during synthesis.  There are other synthesis tools that can synthesize the complete definition of `append`, for example, given `append`'s type signature along with tests that properly cover the behavior of `append`.  (In fact, Michael Ballantyne has been able to synthesize `append` by integrating types into a tiny Scheme-like languge, which I'd like to explore in the context of Barliman.)

To me this is a tradeoff.  Barliman is very flexible in its handling of languages and synthesis problems.  At the same time, Barliman's synthesis is slow, which is why the tool is designed to work interactively with a programmer.  I think this is a reasonable tradoff to explore, since there are plenty of dynamically-typed languages in use (Javascript, Python, Ruby, Scheme/Racket/Clojure/Lisp, etc.).  Also, Barliman doesn't require that the user specify every test necessary to synthesize the complete definition of the function being considered, which reduces the burden on the programmer.

In short, Barliman is flexible, and can handle Turing-complete dynamically-typed higer-order languages, and under-specified synthesis problems, but the tradeoff is that Barliman's synthesis is slow.

Barliman works best for big-step operational semantics.  It is possible to implement small-step semantics in Barliman.  However, the synthesis features of Barliman are likely to work poorly compared with semantics written in a big-step style.

Similarly, Barliman works best for side-effect-free languages, such as a pure subset of Scheme.  Once again, Barliman can handle languages with side effects, such as variable mutation.  However, Barliman's synthesis abilities are likely to suffer as a result.

I do not know how large a language, or how large a definition, Barliman can handle in practice.  I will be experimenting with this...

Barliman can be resource hungry.  Given six example programs and a definition, Barliman will launch eight instances of Chez Scheme, all running in parallel.  Barliman tries to kill these processes when they are not needed, but it is possible for these processes to run for long periods of time (like, forever) and take up unbounded amounts of RAM.  

Barliman currently isn't a very good at standard text editing.  For example, anyone used to paredit or structured text editing will miss those features in Barliman, at least for now.  I do want to add these features to Barliman, especially since I expect they will make the synthesis aspects easier to explore.

Barliman currently doesn't support saving or loading files, definitions, tests, or anything else.  I plan to add this feature soon.

Barliman is changing quickly, and definitely contains errors and interface quirks.  To the best of my knowledge none of these problems are inherent in the design of Barliman, or the technology being used for synthesis.  Still, since this is a rapidly evolving prototype, I expect I will be introducing errors about as quickly as I remove them, at least for a while.




---------------------------------------

## How Barliman works

Barliman uses miniKanren (http://minikanren.org/), and a relational Scheme interpreter written in miniKanren (Byrd, Holk, and Friedman, 2012, http://dl.acm.org/citation.cfm?id=2661105 or http://webyrd.net/quines/quines.pdf), to provide real-time feedback to the code editor using program synthesis.

Chez Scheme in turn uses the miniKanren implementation and relational interpreter implementation contained in the `mk-and-rel-interp` directory.

---------------------------------------

## The default "miniScheme" language

(give grammar and semantics for the default language)

---------------------------------------

## Barliman implementation details

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

## Acknowledgements and thanks

Thanks to Michael Ballantyne, Kenichi Asai, Alan Borning, Nada Amin, Guannan Wei, Pierce Darragh, Alex Warth, Michael Adams, Tim Johnson, Evan Czaplicki, Stephanie Weirich, Nehal Patel, Andrea Magnorsky, Reid McKenzie, Emina Torlak, Chas Emerick, Martin Clausen, Devon Zuegel, Daniel Selifonov, Greg Rosenblatt, Michael Nielsen, David Kahn, Brian Mastenbrook, Orchid Hybrid, Rob Zinkov, Margaret Staples, Matt Hammer, Dan Friedman, Ron Garcia, Rich Hickey, Phil Wadler, Matt Might, participants of my 2016 PEPM tutorial on miniKanren, and particants of the 'As We May Thunk' group (http://webyrd.net/thunk.html), for suggestions, encouragement, and inspiration.

Thanks to Kent Dybvig, Andy Keep, and Cisco Systems for releasing Chez Scheme under an open source license.

The definition of `letrec` in the main interpreter is based based on Dan Friedman's code, using the "half-closure" approach from Reynold's definitional interpreters.


Barliman is intended to be an improved version of the very crude 'miniKanren playground' I showed at my 2016 PEPM tutorial on miniKanren: https://github.com/webyrd/minikanren-playground


-------------------------------------

## Barliman TODOs and limitations


TODO:

* define grammar for microScheme (and the other languages) as a miniKanren relation, and use this grammar to separately check and report whether the definition is grammatically correct.
* consider using ulimit or some other capability for keeping the running Scheme processes under control/keep them from using all the RAM and CPU cycles
* consider turning the background of the "guess" pane green, or otherwise indicting the user, when a guess can be made.  Could also potentially change the code in the main definition edit pane, although this may not be friendly.
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

* try adding contracts/properties/specs. For example, for `append`, could add the property that the sum of `(length l)` and `(length s)` must be equal to `(length (append l s))`.  This could work with randomized testing, even for partially-instantiated definitions.  In the case of `length`, would either need to use Oleg numbers, or CLP(FD).
* related to properties, might want generators, such as a `loso` that generates flat lists of symbols, for example, or `lovo`, that generates flat lists of values, or `treevo`, that generates trees of values.  Could use these generators for specifying and testing properties.  One simple, "type" property is that `append` should work on any two `lovo`s, and, in this case, return of `lovo`.  Could extend this to talk about the lengths of the `lovo`s, etc.  Could then either enumerate or randomly generate `lovo`s QuickCheck style to try to find counter-examples with respect to the current partial (or complete) definition, or perhaps to help with synthesizing the actual code.
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
* explore other synthesis techniques, model checking, etc., as alternatives or additions to the miniKanren-based program synthesis in Barliman
* add tree automata support to support grammars
* add abstract interpretation for miniKanren to speed up the synthesis
* use stochastic/probabilistic extensions to miniKanren to improve synthesis capabilities.  For example, see:

 Eric Schkufza, Rahul Sharma, and Alex Aiken. 2013. Stochastic superoptimization. In Proceedings of the eighteenth international conference on Architectural support for programming languages and operating systems (ASPLOS '13). ACM, New York, NY, USA, 305-316. DOI=http://dx.doi.org/10.1145/2451116.2451150 
https://cs.stanford.edu/people/sharmar/pubs/asplos291-schkufza.pdf


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
