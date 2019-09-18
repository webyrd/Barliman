Relational Abstract Interpretation in Clojure
====

Meow!

webyrd:~/github/rai-clojure/rai-clojure$ lein repl
nREPL server started on port 56364 on host 127.0.0.1 - nrepl://127.0.0.1:56364
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 9.0.1+11
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (require 'clojure.core.logic)
nil
user=> (load "rai_clojure/rai")
nil
user=> (in-ns 'rai-clojure.rai)
#object[clojure.lang.Namespace 0x330a5dea "rai-clojure.rai"]
rai-clojure.rai=> (run 100 [e v] (evalo e v))




webyrd:~/github/rai-clojure/rai-clojure$ lein repl
nREPL server started on port 56058 on host 127.0.0.1 - nrepl://127.0.0.1:56058
REPL-y 0.3.7, nREPL 0.2.12
Clojure 1.9.0
Java HotSpot(TM) 64-Bit Server VM 9.0.1+11
    Docs: (doc function-name-here)
          (find-doc "part-of-name-here")
  Source: (source function-name-here)
 Javadoc: (javadoc java-object-or-class-here)
    Exit: Control+D or (exit) or (quit)
 Results: Stored in vars *1, *2, *3, an exception in *e

user=> (require 'clojure.core.logic)
nil
user=> (load "rai_clojure/rai")
nil
user=> (in-ns 'rai-clojure.rai)
#object[clojure.lang.Namespace 0x11dd7c82 "rai-clojure.rai"]
rai-clojure.rai=> (run 1 [q] (lookupo 3 [[1 2] [3 4] q))

RuntimeException Unmatched delimiter: )  clojure.lang.Util.runtimeException (Util.java:221)
RuntimeException Unmatched delimiter: )  clojure.lang.Util.runtimeException (Util.java:221)
rai-clojure.rai=> (run 1 [q] (lookupo 3 [[1 2] [3 4] q))

RuntimeException Unmatched delimiter: )  clojure.lang.Util.runtimeException (Util.java:221)
RuntimeException Unmatched delimiter: )  clojure.lang.Util.runtimeException (Util.java:221)
rai-clojure.rai=> (run 1 [q] (lookupo 3 [[1 2] [3 4]] q))
(4)
rai-clojure.rai=> (run 1 [q] (lookupo 5 [[1 2] [3 4]] q))
()
rai-clojure.rai=> (run 1 [q] (lookupo w [[w 2] [v 4]] q))

CompilerException java.lang.RuntimeException: Unable to resolve symbol: w in this context, compiling:(/private/var/folders/wm/4zr1t8m911lcrz9z7c54n4nm0000gn/T/form-init11060800822347525331.clj:1:12)
rai-clojure.rai=> (run 1 [q] (lookupo 'w [[w 2] [v 4]] q))

CompilerException java.lang.RuntimeException: Unable to resolve symbol: w in this context, compiling:(/private/var/folders/wm/4zr1t8m911lcrz9z7c54n4nm0000gn/T/form-init11060800822347525331.clj:1:12)
rai-clojure.rai=> (run 1 [q] (lookupo 'z [[w 2] [v 4]] q))

CompilerException java.lang.RuntimeException: Unable to resolve symbol: w in this context, compiling:(/private/var/folders/wm/4zr1t8m911lcrz9z7c54n4nm0000gn/T/form-init11060800822347525331.clj:1:12)
rai-clojure.rai=> (run 1 [q] (lookupo 'z [['w 2] ['v 4]] q))
()
rai-clojure.rai=> (run 1 [q] (lookupo 'v [['w 2] ['v 4]] q))
(4)
rai-clojure.rai=>


