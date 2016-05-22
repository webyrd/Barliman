# miniKanren-with-symbolic-constraints

A revision of https://github.com/webyrd/miniKanren-with-symbolic-constraints/ for better performance. Up to 10x faster for large queries involving heavy use of constraints.

Includes `==`, `=/=`, `symbolo`, and `numbero`. `absento` is included, but the argument is required to be an eqv-comparable ground atom.

Eigen was removed.

## Running

### Racket

```
(require "mk.rkt")
```

### Vicare

```
(load "mk-vicare.scm")
(load "mk.scm")
```

## Running Tests

After loading miniKanren as above,

```
(load "test-all.scm")
```

regardless of scheme implementation.
