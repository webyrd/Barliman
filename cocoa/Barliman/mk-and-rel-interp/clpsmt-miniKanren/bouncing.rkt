#lang racket

(require "mk.rkt")
(do-defer-smt-checks!)
(require 2htdp/universe 2htdp/image)

(provide (all-from-out "mk.rkt")
         (all-defined-out))

(define maxw 400)
(define maxh 400)

(define (is-boxo b x y w h)
  (fresh ()
    (z/assert `(<= 0 ,x))
    (z/assert `(<= 0 ,y))
    (z/assert `(<= 1 ,w))
    (z/assert `(<= 1 ,h))
    (z/assert `(<= (+ ,x ,w) ,maxw))
    (z/assert `(<= (+ ,x ,w) ,maxh))
    (== b `(box ,x ,y ,w ,h))))

(define (make-boxo w h b)
  (fresh (x y)
    (is-boxo b x y w h)))

(define box-x cadr)
(define box-y caddr)
(define box-w cadddr)
(define (box-h x) (cadddr (cdr x)))
(define (boxo-w b w)
  (fresh (x y h)
    (is-boxo b x y w h)))
(define (boxo-x b x)
  (fresh (y w h)
    (is-boxo b x y w h)))
(define (boxo-y b y)
  (fresh (x w h)
    (is-boxo b x y w h)))


(define (draw-scene b)
  (place-image
   (rectangle (box-w b) (box-h b) "solid" "gray")
   (box-x b) (box-y b)
   (empty-scene maxw maxh "white")))

(define TICK-RATE 1)

(define (ex1)
  (let ((r (run 10 (b) (make-boxo 40 40 b))))
    (big-bang r
              (on-tick cdr TICK-RATE)
              (stop-when null?)
              (to-draw (lambda (x) (draw-scene (car x)))))))

(define ((moveo t) b)
  (fresh (x)
    (make-boxo 40 40 b)
    (boxo-x b x)
    (z/assert `(<= (* ,t 20) ,x))
    (z/assert `(>= (* (+ 1 ,t) 20) ,x))))

(define (move2 t)
  (run 1 (b) ((moveo t) b)))

(define (ex2)
  (big-bang 0
            (on-tick (lambda (t) (+ t 1)) TICK-RATE)
            (stop-when (lambda (t) (null? (move2 t))))
            (to-draw (lambda (t) (draw-scene (car (move2 t)))))))

(define ((moveo3 t) b)
  (fresh (y) ((moveo t) b) (boxo-y b y) (z/assert `(= ,y 100))))

(define (move3 t)
  (run 1 (b) ((moveo3 t) b)))

(define (ex3)
  (big-bang 0
            (on-tick (lambda (t) (+ t 1)) TICK-RATE)
            (stop-when (lambda (t) (null? (move3 t))))
            (to-draw (lambda (t) (draw-scene (car (move3 t)))))))
  
(define (ex4)
  (let ((r (run 10 (b) (fresh (t) ((moveo3 t) b)))))
    (big-bang r
              (on-tick cdr TICK-RATE)
              (stop-when null?)
              (to-draw (lambda (x) (draw-scene (car x)))))))

(define (runt t tq g)
  (conde
    ((z/assert `(= ,tq ,t)) (g))
    ((z/assert `(not (= ,tq ,t))) (runt (+ 1 t) tq g))))

(define (with-toggled-get-next-model?! t)
  (toggle-get-next-model?!)
  (let ((r (t)))
    (toggle-get-next-model?!)
    r))

(define (ex5)
  (let ((r (with-toggled-get-next-model?! (lambda () (run 10 (b) (fresh (t) (z/assert `(<= 0 ,t)) (runt 0 t (lambda () ((moveo3 t) b)))))))))
    (big-bang r
              (on-tick cdr TICK-RATE)
              (stop-when null?)
              (to-draw (lambda (x) (draw-scene (car x)))))))
