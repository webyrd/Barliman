# Barliman

Joint work with Greg Rosenblatt (https://github.com/gregr).

---------------------------------------

"He thinks less than he talks, and slower; yet he can see through a brick wall in time (as they say in Bree)."

--Gandalf the Grey, about Barliman Butterbur

The Lord of the Rings: The Fellowship of the Ring

J. R. R. Tolkien

---------------------------------------

## Barliman News

4 December 2016

* The first talk on Barliman, presented by Will Byrd and Greg Rosenblatt at Clojure/conj 2016, is now online: https://www.youtube.com/watch?v=er_lLvkklsk  Here is the example code from the talk: https://github.com/webyrd/Barliman/tree/master/talk_examples_20161203

* There is now a Barliman Google Group: https://groups.google.com/d/forum/barliman-editor

* We are starting a Google Hangout series on Barliman, relational interpreters, and program synthesis.  Please sign up for the Barliman Google Group if you are interested in joining us!

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

Here are a few screenshots of Barliman, using the Mac implementation as of June 4, 2016.  (Update:  I've added a few newer screenshots from June 16, 2016.  Once Barliman stops changing as rapidly I'll update all the screenshots.)  (Update, 10 October 2016:  Please see the `interesting_examples` directory for more recent examples and screenshots.)  (Update, 4 December 2016:  The Clojure/conj 2016 talk is currently the best source of up-to-date information on Barliman: https://www.youtube.com/watch?v=er_lLvkklsk)

The first screenshot shows the main editor window.  The `Scheme Definition` edit pane contains the complete (fully instantiated) and correct definition of `append`, the list concatenation function in Barliman's default "miniScheme" language.  `append` will be our simple running example in these screenshots.  The edit window also contains three tests; each test contains an input expression, and the expected value of that expression.  The `Best Guess` pane, which is not editable by the user, contains the same fully instantiated definition of `append` as in the `Scheme Definition` edit pane.

All the text in the editor window is black, indicating that all the information in the editor is consistent and valid.  The definition of `append` is a valid symbolic expression (s-expression), and is indeed a syntactically valid miniScheme definition.  The test expressions and expected values are syntactically valid, and consistent with each other.

The editor window displayed in this first screeenshot is similar in spirit to a modern integrated development environment (IDE) that runs tests whenever the code to be tested is modified.

Let's see how we might have gotten to the final version of `append` using Barliman.

#### screenshot 1: Fully instantiated definition of append

![append example 1 -- fully instantiated code](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append01.jpg "append example 1 -- fully instantiated code")



Screenshot 2 shows the empty main editor window, immediatly after starting Barliman.  We know we want to define `append`, so in true test-drived development style we begin by writing our tests cases.


#### screenshot 2: 

![append example 2 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append13.jpg "append example 2 -- ")



Screenshot 3 shows the main editor window with our three tests.  The first test says that if we append the empty list to the empty list, we should get back the empty list.  You should be able to figure out the other two tests.

The text for all three tests are red, indicating that none of the tests pass.  This isn't surprising, perhaps, since we haven't started to define the `append` function.

(From an interface design standpoint, whether to use colors, which colors to use, which text fields to hilight, etc., are all open questions in my mind.  Over time I hope to make it much more clear exactly which part of the code is failing, and why.)

#### screenshot 3: 

![append example 3 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append09.jpg "append example 3 -- ")



Screenshot 4 shows the main editor window after we have begun defining `append` in the `Scheme Definition` edit pane.  Our parentheses are not balanced -- we haven't yet typed a closing parenthesis for the `define` form.  Because of the missing parenthesis, the definition is not a legal Scheme s-expression.  The tests cannot pass, of course, since `append` isn't even an s-expression.  Barliman recognizes this, and turns the text in the `Scheme Definition` edit pane, and the text in the test edit fields, a sickly green color.

(Future versions of Barliman should include a structured editor that will automatically insert balanced parentheses.)

#### screenshot 4: 

![append example 4 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append11.jpg "append example 4 -- ")



Screenshot 5 shows the main editor window after we have added the closing parenthesis in our partial definition of `append` in the `Scheme Definition` edit pane.  The partial definition of `append` is now a legal s-expression.  However, the definition of `append` is not syntactically valid according to the rules of miniScheme.  Of course, this invalid definition of `append` causes all the tests to fail as well.  Barliman recognizes this, and turns the text in the `Scheme Definition` edit pane, and the text in the test edit fields, red.

(Currently Barliman doesn't actually check that definitions are grammatically correct.  Rather, Barliman uses evaluation of the tests to check whether code is *semantically* legal, rather than syntactically legal.  Future versions of Barliman will probably include explicit grammars that are checked, in addition to semantic rules.)  (Update: Barliman now includes a relational parser for the miniScheme language, as is shown in the newer screenshots at the end.)

#### screenshot 5: 

![append example 5 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append10.jpg "append example 5 -- ")


In screenshot 6 we can see that the programmer has partially specified the defintion of `append`.  The definition is a syntactally-correct s-expression, and indeed is a syntactically correct use of miniScheme's `define` form.  Importantly, the definition of `append` is only partially specified, and contains four (logic) variables (`A`, `B`, `C`, and `D`) representing unknown subexpressions.  In Barliman variables representing unknown subexpressions are single-letter upper-case variables `A` through `Z`.  (Note to Schemers: The comma (`,`) that usually occurs before these letters is necessary because the code in the `Scheme Definition` edit pane is implicitly quasiqoted.)

Given the partially-specified defintion of `append` in the `Scheme Definition` edit pane, along with the three tests, Barliman is able to correctly "guess" the code corresponding to these variables.  The correct and complete definition of `append` is displayed in the `Best Guess` pane.  Barliman guesses the correct code in this case in a second or less.  All of the text in the main editor window is black, indicating that all of the code is syntactically correct, and that all three tests pass given the completed definition of `append` shown in the `Best Guess` pane. 

#### screenshot 6: 

![append example 6 -- partially instantiated code filled in](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append07.jpg "append example 6 -- partially instantiated code filled in")



Screenshot 7 shows an *incorrect* partial definition of `append`.  As in the previous screenshot, the partial definition of `append` contains variables representing unknown subexpressions (the `A` and `B` and `C`).  However, in this case the first argument to `cons` is incorrect.  The first argument to `cons` should be `(car l)`, as shown in screenshot 1.  Alternatively, the first argument to `cons` could be an incomplete expression containing a variable representating an unknown subexpression, such as `(car ,B)` from screenshot 6, provided that this incomplete expression is *consistent* with the expression `(car l)`.  Here, however, the first argument to `cons` is the expression `(cdr l)`.  The red text for tests 2 and 3 indicate that these tests are *incompatible* with the partial definition of `append` in the `Scheme Definition` edit pane.  That is, there are no legal miniScheme expressions that could be substituted for the variables `A`, `B`, and `C` that would make tests 2 and 3 pass.

The spinning progress indicator to the upper-right of the `Best Guess` pane indicates that Barliman is trying to find expressions for variables `A`, `B`, and `C` that will make all of the tests pass.  Of course this is impossible -- Barliman should be a little smarter, and cut off the `Best Guess` computation when one of the individual tests fails.

The important thing about this example is that Barliman was able to prove that the partial definition of `append` is incorrect, without `append` being fully defined.  More precisely, the partial definition of append is inconsistent with respect to tests 1 through 3 *and* the semantics of miniScheme (which can be edited by the programmer).

#### screenshot 7:

![append example 7 -- partially instantiated code incompatible with tests](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append08.jpg "append example 7 -- partially instantiated code incompatible with tests")






Screenshot 8 shows another partially-instantiated, but incorrect, definition of `append`.  The base case of `append` should be `s` instead of `l`, yet all the text is in black, indicating that the individual tests are compatible with the definition so far.  The problem is that we don't have a test that exposes that this partial definition is wrong.  We'll fix this in the next screenshot.

This is one danger of using tests for feedback, of course -- in general, no finite number of tests is sufficient to prove our definition is correct.  I hope that future versions of Barliman will include other ways to specify the behavior of programs, which might include specifying program properties, or providing a "reference" implementation of a function that is being redefined to perform faster, etc.


#### screenshot 8:

![append example 8 -- partially instantiated code missing a test](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_08/append19.jpg "append example 8 -- partially instantiated code missing a test")


In screenshot 9 we add a new test, test 4, that shows that the base case of `append` is incorrect.  Sure enough, test 4's text immediately turns red, indicating it is incompatible with our partial definition.

#### screenshot 9:

![append example 9 -- partially instantiated code with the missing test](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_08/append20.jpg "append example 9 -- partially instantiated code with the missing test")







Screenshot 10 shows a limitation of Barliman's program synthesis.  Here the partially-specified definition of `append` contains only a single variable, `A`, representing an unknown subexpression.  Ideally Barliman would quickly figure out that `A` should be the expression `(cdr l)`.  However, for this example Barliman seems to get "stuck" -- we can see the spinning progress indicators to the upper-right of the `Best Guess` pane and the `Test 2` and `Test 3` edit fields, indicating that Barliman is still "thinking".  I let Barliman run for a minute or two, but it didn't find a value for `A` in that time.  (Adding the notion of "parsimony" to Barliman, so it tries to generate the smallest terms first, might help with this example.)

We could allow Barliman to keep thinking -- perhaps it would find the answer in five minutes, or in an hour (provided our computer has enough RAM!).  However, in practice we would probably try filling in `A` manually.  If we were to type `(cdr ,B)` in place of `,A`, Barliman would immedialy guess in the correct, trivial subexpression `l` for the variable `B`.

This example shows how program synthesis in Barliman can be much slower than we might hope in certain cases.  However, since Barliman is a text editor, and since multicore computers with lots of RAM are now ubiquitous, I see these examples from a "glass half full" perspective.  Sometimes Barliman can help you, either by guessing the rest of your incomplete definition, or by proving that there is no completion for your partially-specified definition that is consistent with your tests.  In this case you win.  Sometimes Barliman can't help you, in which case you use it like a regular text editor.  In this case you use more CPU cyles and RAM on your machine, but otherwise edit text normally.

Of course, Barliman isn't currently a particularly *good* text editor, especially compared to Emacs with paredit mode, to take one example.  This problem is only a matter of engineering -- in fact, Barliman-like functionality could be added to Emacs, or to another editor.  Or Barliman could get more sophisticated editing abilities.

A bigger drawback is that the semantics for the language you are writing in must be specified in miniKanren.  This is fine if you are writing in a minimal Scheme subset, such as miniScheme.  This isn't so great if you want to program in full Clojure or Racket or Javascript or Ruby.  Finding ways to scale this technology is an open problem.  The solution may not be miniKanren or constraint logic programming, but rather another synthesis approach.  I don't know.  I do hope, however, that Barliman will make people think about how synthesis capabilities can be integrated into editors, especially for dynamic languages.

#### screenshot 10: 

![append example 10 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_03/append12.jpg "append example 10 -- ")


---------------------------------------

Update: Here are a few newer screenshots, as of June 16, 2016, that show off the relational parser for miniScheme that I added a couple of days ago.

Screenshot 11 is an updated version of screenshot 5, showing the new relational parser at work.  The definition of `append` is syntactically incorrect, which is represented by the purple text in the `Scheme Definition` edit pane.  The tests are syntactically correct, but fail, and are therefore shown in red text.

#### screenshot 11:

![append example 11 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_16/append23.jpg "append example 11 -- ")

Screenshot 12, like screenshot 11, shows a syntactically incorrect definition of `append`.  In this case the keyword `lambda` appears as the body of a `lambda` expression.  In miniScheme, as in Scheme, `lambda` is a special form rather than a function; the keyword `lambda` cannot appear "by itself".  Hence the purple text in the `Scheme Definition` edit pane.

Once again, the tests are syntactically valid, but fail, and so are shown in red text.

#### screenshot 12: 

![append example 12 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_16/append21.jpg "append example 12 -- ")

Screenshot 13 is identical to screenshot 12, except that the formal parameter to the `lambda` expression in the `Scheme Definition` edit pane has been changed from `x` to `lambda`.  This formal parameter *shadows* the `lambda` keyword, allowing `lambda` to appear by itself in the body of the `lambda` expression.

Once again, the tests are syntactically valid, but fail, and so are shown in red text.

This example shows that the relational parser keeps track of the environment, variable scope, and shadowing.

#### screenshot 13:

![append example 13 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_16/append22.jpg "append example 13 -- ")

Screenshot 14 shows a syntactically legal partial definition of `append` in the `Scheme Definition` edit pane.  Three of the tests are syntactically legal, and are (individually) consistent with the partial definition of `append`; therefore, the text for these tests are shown in black.

The third test, however, is syntactically incorrect.  This is because in miniScheme, as in Scheme, `and` is a special form rather than a function, and therefore cannot be passed into the call to `append`.  Since the third test is syntactically illegal, it is shown in purple text.

#### screenshot 14:

![append example 14 -- ](https://github.com/webyrd/Barliman/blob/master/screen_shots/2016_june_16/append24.jpg "append example 14 -- ")

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

As in Scheme, in miniScheme duplicate variable names of definitions at the same scoping level, or duplicate `lambda` or `letrec` bindings, are illegal.  However, Barliman does not currently detect these violations.  For example, Barliman will not complain about the expression `((lambda (x x) x) 3 4)`, the behavior of which is unspecified.  Probably the parser should enforce that the variable names are distinct.

The `lambda` and `letrec` forms do not contain an implicit `begin`.

The `lambda` form supports multiple arguments, `(lambda (x y z) y)`, and a single "variadic" argument, `(lambda x x)`, but currently doesn't support the full Scheme variadic syntax, `(lambda (x y . z) x)`.

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

Thanks to Michael Ballantyne, Kenichi Asai, Alan Borning, Nada Amin, Guannan Wei, Pierce Darragh, Alex Warth, Michael Adams, Tim Johnson, Evan Czaplicki, Stephanie Weirich, Molly Feldman, Joe Osborn, Nehal Patel, Andrea Magnorsky, Reid McKenzie, Emina Torlak, Chas Emerick, Martin Clausen, Devon Zuegel, Daniel Selifonov, Greg Rosenblatt, Michael Nielsen, David Kahn, Brian Mastenbrook, Orchid Hybrid, Rob Zinkov, Margaret Staples, Ziyao Wei, Matt Hammer, Hunter Hutchinson, Bryan Joseph, Cesar Marinho, Michael Bernstein, Bodil Stokke, Dan Friedman, Ron Garcia, Rich Hickey, Phil Wadler, Tom Gilray, Dakota Fisher, Gavin Whelan, Devon Zeugel, Jonas Kölker, Matt Might, participants of my 2016 PEPM tutorial on miniKanren, and particants of the 'As We May Thunk' group (http://webyrd.net/thunk.html), for suggestions, encouragement, and inspiration.

Thanks to Kent Dybvig, Andy Keep, and Cisco Systems for releasing Chez Scheme under an open source license.

The definition of `letrec` in the main interpreter is based based on Dan Friedman's code, using the "half-closure" approach from Reynold's definitional interpreters.

Greg Rosenblatt has been improving the search and the miniScheme interpreter to improve synthesis performance, greatly improving performance on many of the synthesis problems.

Barliman is intended to be an improved version of the very crude 'miniKanren playground' I showed at my 2016 PEPM tutorial on miniKanren: https://github.com/webyrd/minikanren-playground


-------------------------------------

## Barliman TODOs and limitations


TODO:

* Matt Might suggests using properties like `(append (cons a l) s) == (cons a (append l s))` for synthesizing `append`.
* Devon Zeugel suggests using a monospace font, and perhaps swapping the tests and the definitions in the main window.  Devon suggests using the number of tests that pass to guide search for auto-repair or other synthesis.  Or, to try to maximize the number of tests that pass, then have the user take over.  Maybe use the number of tests that pass as a score/heuristic for stochastic search.
* Tom Gilray suggests being able to hover over a ,A logic variable to select/approve a suggested/guessed value for that particular subexpression.  Michael Ballantyne and other people have suggested similar notions, including a scrubber for scrubbing over/selecting a generated/guessed value for a subexpression.
* replace test input/output edit fields with multi-line edit capabilities similar to that of the 'Definitions' pane
* add paren hilighting to editor
* add "smart delete" of parens
* add add auto-indent
* add forward/backward s-expression
* add transpose s-expression
* add pretty printing of "Best Guess" definitions
* add smart editing/auto insert of gensyms in the test edit panes, similar to how smart editing/auto insert of logic variables works in the Definitions edit pane
* for 'syntax error' and 'illegal sexpression' messages for a test, potentially show whether the input, the output, or both is the problem (could be complicated in that the output might be an illegal sexpression, while the input is a syntax error, for example)
* have Barliman attempt to guess the result of a test, as the programmer types in the test (thanks Ziyao Wei!)
* show the definition guessed for each individual successful test
* show reified test inputs and outputs upon success, for all tests (would allow queries like 'quines')
* mouse hover over ,A variable should display the variable's "Best Guess" value
* allow resizing of Barliman main window
* add `let` and `cond`.
* add better error message for 'invalid syntax', at least indicating whether there is an unexpected paren/missing end paren
* Possibly replace `list` call in the "best quess" query with nested `cons` calls instead.  (Need to time this again with Greg's new improvements to the search.)  This can be an order of magnitude faster in some cases, according to my testing (variadic application is more expensive than 'cons' in the current miniScheme interpreter, apparently: see times for append-gensym-synthesis-with-cons-1 versus append-gensym-synthesis-with-list-1 tests in test-interp.scm).
* add an implicit `begin` to `lambda`, `letrec`, and `let` forms.
* parser should enforce that the variable names are distinct in `lambda` formals, `letrec` bindings and formals, and `define`'s within the same scope.
* create a version of Barliman on an open platform (Electron, Clojurescript, Lighttable, whatever).  Any help would be appreciated!  :)
* consider using ulimit or some other capability for keeping the running Scheme processes under control/keep them from using all the RAM and CPU cycles
* consider adding fields for seeing the ground results of partially-instantiated test inputs/outputs
* add full variadic syntax: `(lambda (x y . z) x)`
* consider turning the background of the "guess" pane green, or otherwise indicting the user, when a guess can be made.  Could also potentially change the code in the main definition edit pane, although this may not be friendly.
* add STLC as an example, complete with type inferencer
* perhaps be able to drag and drop subexpressions from the best guess pane onto variables in the definition pane.  And also be able to replace an extort subexpression in the definition pane with a logic variable.
* think about contextual menus/right click and also drag and shift-drag.  What should these do?
* make sure Semantics and the main Barliman windows can be reopened if the user closes them!  Currently there doesn't seem to be a way to get the window back.  Perhaps allow the user to hide the windows, but not close them?  What is the preferred Mac way?
* for the case in which a simple function is being used to generate test inputs and answers for a more complex version of the same function, may need or want a grounder to make sure answers are fully ground.  May also want a grounder for code, esp for the best guess pane.  Although grounding code may not be necessary or ideal.
* would be smart to only re-run Scheme processes when the Scheme code actually *changes* -- for example, white space characters outside of an S-expr shouldn't trigger re-evaluation.  One way would be to compare "before" and "after" S-exprs to see if anything has changed.  Could run a single Scheme instance and call `equal?` to see if the code has actually changed.  This could be a big win for expensive computations.
* add ability to save and load examples/tests/semantics, and include interesting examples, such as a tiny Scheme interpreter written in Scheme, state machine using mutual recursion, examples from pearls, etc.
* add structured editor for semantics and for type inferencer (as an alternative to/in addition to the free-form editor)
* possibly move as much work as possible into NSTasks, such as loading files.
* possibly add pairs of tests as processes, once individual tests complete successfully
* add syntax-directed auto-indentation of code
* figure out how to do syntax-directed hilighlighting, and precise hilighting of syntax errors.  May not be as important if I go the structured editor route.  Although perhaps this should be an option, either way.
* add documentation/tutorial
* add paper prototype for desired features
* move 'barliman-query.scm' temporary file to a more suitable location than 'Documents' directory, or get rid of the temp file entirely
* experiment with store passing style and small step interpreters
* get rid of hardcoded path to Chez executable
* add input/output examples
* find a cleaner and more flexible way to construct the program sent to Chez
* add "accept suggested completion" button
* would be smarter/less resource intense to not launch all the tests again when the text in a single test changes.  Only that test and allTests need be re-run, in theory.  Getting the UI to display the state of everything properly may be a little subtle, though.
* differential relational interpreters
* use a meta-interpreter to let the programmer know the deepest part of the search path upon failure, to try to give a better hint as to what went wrong (thanks Nada! and halp! :))

LONGER TERM:

* Devon Zeugel suggested looking at Mutant (https://github.com/mbj/mutant).
* mousing over a failing test should highlight subexpressions in the Definitions pane that are incompatible with that test.
* mousing over a subexpression should hilight any tests which would be incompatible with the definitions, were a logic variable to be substututed for the expression being moused over. (perhaps do this only if a modifier key is help down)
* improve editor so that typing '(' 'cons' auto completes to '(cons ,A ,B)', based on arity of cons (unless cons is shadowed).
* consider placing each of the 'definition' forms in its own edit window, with 'uses mutattion', 'uses call/cc', 'well-typed' checkboxes for each definition (inspired by Kenichi Asai's tool for teaching functional programming).
* try adding contracts/properties/specs. For example, for `append`, could add the property that the sum of `(length l)` and `(length s)` must be equal to `(length (append l s))`.  This could work with randomized testing, even for partially-instantiated definitions.  In the case of `length`, would either need to use Oleg numbers, or CLP(FD).
* related to properties, might want generators, such as a `loso` that generates flat lists of symbols, for example, or `lovo`, that generates flat lists of values, or `treevo`, that generates trees of values.  Could use these generators for specifying and testing properties.  One simple, "type" property is that `append` should work on any two `lovo`s, and, in this case, return of `lovo`.  Could extend this to talk about the lengths of the `lovo`s, etc.  Could then either enumerate or randomly generate `lovo`s QuickCheck style to try to find counter-examples with respect to the current partial (or complete) definition, or perhaps to help with synthesizing the actual code.
* automatic test generation/fuzzing
* add arithmetic to the main interpreter
* explore incremental computing with the editor
* add type inferencer
* test generation of typed test programs
* partial evaluation of the interpreter to speed up evaluation
* add support for macros
* explore predicates/generators/QuickCheck-like functionality
* explore other synthesis techniques, model checking, etc., as alternatives or additions to the miniKanren-based program synthesis in Barliman
* add tree automata support to support grammars
* add abstract interpretation for miniKanren to speed up the synthesis
* use stochastic/probabilistic extensions to miniKanren to improve synthesis capabilities.  For example, see:

 Eric Schkufza, Rahul Sharma, and Alex Aiken. 2013. Stochastic superoptimization. In Proceedings of the eighteenth international conference on Architectural support for programming languages and operating systems (ASPLOS '13). ACM, New York, NY, USA, 305-316. DOI=http://dx.doi.org/10.1145/2451116.2451150 
https://cs.stanford.edu/people/sharmar/pubs/asplos291-schkufza.pdf



POSSIBLE USE CASES:

* write simple implementation of a function, generate test from that function, then use those tests to guide the more sophisticated implementation.  Or more generally, continually test the partially-implemented function vs the fully implemented but perhaps less efficient function.

* Matt Might suggests as a use case, "automatic program repair after bug discovery," and points to work by Stephanie Forrest.  I really like this idea.  Here's how the use case might work:

You write tests and code. The tests pass. Later you find an error in the code, so you go back and add more tests, which fail.

Click a Barliman 'auto-repair' button. Barliman tries, in parallel, removing each subexpression and trying synthesis to fill in the rest.

If Barliman could use a Amazon server with dozens of hardware cores and 2TB RAM (like the new X1 server on AWS), this really could be done in parallel.

Or run locally until there's a timeout, then run again with the holes in other places. Could even try pairs of holes to keep the synthesis problem as small as possible.

Or, perhaps more practical short term until Barliman's synthesis improves...

Have Barliman try removing each subexpression and then check if any of the tests still fail. Then hilight these known bad subexpressions to help guide the user.

Greg Rosenblatt's suggestion for auto-repair: "The user may also want to mark some regions of the code as suspect, which would prioritize the area searched for problematic sub-expressions.  If the user is right, the fix could be found much sooner."



SUSPECT IDEAS:

* could just call out to Scheme one the program becomes grounded.  However, the semantics and even the grammar may not match that of the interpreter used by miniKanren, so this seems difficult or impossible to do properly.  However, could call a (non-relational) interpreter for miniScheme.


INTERESTING IDEAS:

* Tom Gilray suggests using a simplified intermediate representation (IR) that disallows shadowing, has `if` but not `cond`, etc.  Could have the IR be the macro expanded code.  Could possibly reverse engineer/infer macro calls that could have produced the IR.
* Tom Gilray suggests changing the interface to just have a single editor window, which allows for definitions and for test calls/results.  I suspect this is the right way to go, although it will involve significant changes to Barliman.  Tom also suggests having arrows to the right of each logic variable, showing the current value of each variable. 
* perhaps use delayed goals to implement arithmetic over floating point numbers, and other tricky operations.  If the arguments do not become instantiated enough, Barliman should be non-commital (can't synthesize code, and can't prove tests are not consistent with the code until the code is more instantiated).
* Greg Ronsenblatt suggests dividing tests into a 'training' set and a 'test' set, as is done in machine learning to avoid overfitting.  Of course this could also lead into 'propety-based testing', generators, etc.
* Jonas Kölker suggests synthesizing multiple definitions, or perhaps even all of the Haskell Prelude-style list functions, simultaneously, based on the relationships between the functions, and their Quickcheck-style properties.  He also suggests using properties like `reverse (xs++ys) == reverse ys ++ reverse xs` and `map f (xs++ys) == map f xs ++ map f ys` for synthesis.


KNOWN LIMITATIONS:

* The `Best Guess` pane cannot be resized vertically, which sometimes cuts off text.
* Non-specific error indication (the color changes for the text, but does not show which part of the text caused the error)
* Currently the UI only supports 6 tests.  Should allow more tests to be added.
* Test inputs and outputs are `NSTextField`s rather than `NSTextView`s, which makes writing longer and more complicated tests awkward.


KNOWN ERRORS:

* Shadowing of syntax is no longer working.  (and and #t) => #t should result in a syntax error, unless the Definitions pane contains a defn named 'and': (define and (lambda x  x)).  In which case, (and and #t) should be legal syntax.  (Wonder if we broke this when we messed with the environment in evalo.)
* It is possible, rarely, to exit Barliman and still have a Scheme process running in the background.  Need a way to better track which processes have been started and make sure to kill them.  Or potentially use something like `ulimit` when launching a process.
* The miniKanren queries constructed by Barliman expose several local variable names and a number of global variable names that could accidentally or intentionally be used by the programmer.  Need to tighten this up.
* closing one of the windows means the window cannot be reopened!  Oops.  I'm not going to worry about this until I decide what to do with the Semantics window.
* sometimes the spinners stop spinning but are still visible

DONE (features on the TODO list implemented since the original release of Barliman)

* Implemented monospace font, as recommended by Devon Zeugel.
* Fixed error: An illegal s-expression in the 'Definitions' edit pane will make test input/output expressions that are legal expressions appear to be illegal.
* Fixed error (by removing auto-insert of right parens): auto-insert of right parens and auto-insert of logic variables breaks 'undo'.  Guess I need to learn how 'undo' works in Cocoa...
* Fixed error: The '0' key no longer seems to work.
* Fixed error: (lambda (,A ,B) ...) should always produce a disequality constraint between A and B.
* Fixed error: Will modified the miniKanren reifier to remove unnecessary constraints involving gensyms.  Alas, he goofed it, and the reifier removes too many constraints, including some =/= and absento constraints in play when gensyms are used.
* Fixed error: scoping problem introduced by optimizing variable lookup.
* Fixed error: Inserting a new logic variable with Control-<space> doesn't replace highlighted text.  Instead, it adds the variable and keeps the hilighted text.
* changed reifier and main edit window to display constraints separately from "best guess" definition(s).
* fixed: quickly tab-deleting a test with a syntax error (for example) can leave the 'syntax error message' on an empty test
* added automatic addition of right parens, and auto-addition of logic variables (thanks Michael Ballantyne, Guannan Wei, Pierce Darragh, Michael Adams, for discussions on how this might work)
* changed reifier so that constraints involving gensym are not displayed.
* Fixed error: even if the "best guess" query terminates with success, individual test processes may still keep running, since those tests don't "know" that the best guess involving *all* the tests has succeed.  If the best guess query terminates with success, the individual test processes should be killed, and marked as successful (black text, stop the progress spinner).
* updated `letrec` to allow for zero or more bindings, and updated `begin` to allow for zero or more definitions; this allows the creation of mutually-recursive functions.
* define grammar for microScheme (and the other languages) as a miniKanren relation, and use this grammar to separately check and report whether the definition is grammatically correct.
* cancel the allTests operation if any single test fails, since in that case allTests cannot possibly succeed
* wait part of a second to see if there are more keystrokes before launching Scheme processes.  Sort of like XCode (I assume XCode is doing this).  Would be more resource friendly, less distracting, and would make typing quickly more responsive.  Could probably do this using an timer.
* add ability to change the evaluator rules and perhaps an explicit grammar as well
