#lang racket

(require 2htdp/universe 2htdp/image)
(require racket/system)

(define (model-assoc m)
  (map (lambda (x) (match x [(list _ lhs _ _ rhs) (cons lhs rhs)])) (cdr m)))
(define (get x m) (cdr (assoc x m)))

(define (ex-scene m)
  (place-image
   (rectangle 40 40 "solid" "gray")
   (get 'x m) 100
   (empty-scene 400 400 "white")))
(define (ex-constraints say)
  (say '(declare-const t Int))
  (say '(declare-const x Int))
  (say '(assert (= x (* 10 t))))
  (say '(assert (<= 0 x)))
  (say '(assert (<= x 400))))


(define (run-world constraints scene)
  (match (process "z3 -in")
    [(list p-in p-out _ p-err p-fun)
     (define (say msg)
       (fprintf p-out "~a\n" msg))
     (define (check-sat)
       (say '(check-sat))
       (flush-output p-out)
       (let ((r (read p-in)))
         (println r)
         (eq? r 'sat)))
     (define (get-model)
       (say '(get-model))
       (flush-output p-out)
       (model-assoc (read p-in)))
     
     (constraints say)
     (big-bang 0
               (on-tick (lambda (t) (+ t 1)))
               (stop-when (lambda (t) (= t 40)))
               (to-draw (lambda (t)
                          (say '(push)) 
                          (say `(assert (= t ,t)))
                          (check-sat)
                          (let ((m (get-model)))
                            (println m)
                            (say '(pop))
                            (scene m)))))

     (close-input-port p-in)
     (close-output-port p-out)
     (close-input-port p-err)]))

(run-world ex-constraints ex-scene)
