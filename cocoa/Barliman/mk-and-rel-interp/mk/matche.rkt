#lang racket
(require "mk.rkt")
(require (for-syntax racket/syntax))

(provide matche lambdae defmatche)

(define-for-syntax memp memf)

(include "matche.scm")
