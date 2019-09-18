#lang racket

(require 2htdp/universe 2htdp/image)
(require racket/system)

(define (model-assoc m)
  (map (lambda (x) (match x [(list _ lhs _ _ rhs) (cons lhs rhs)])) (cdr m)))
(define (get x m) (cdr (assoc x m)))

(define (ex-scene m s)
  (place-image
   (rectangle 40 40 "solid" "gray")
   (get 'x m) 100 s))
(define (ex-constraints say)
  (say '(declare-const t Int))
  (say '(declare-const x Int))
  (say '(assert (= x (* 10 t))))
  (say '(assert (<= 0 x)))
  (say '(assert (<= x 400))))


(define (ex2-scene m s)
  (place-image
   (circle 40 "solid" "red")
   (get 'cx m) (get 'cy m)
   (place-image
    (rectangle 40 40 "solid" "gray")
    (get 'rx m) (get 'ry m)
    s)))
(define (ex2-constraints say)
  (say '(declare-const t Int))
  (say '(declare-const rx Int))
  (say '(declare-const cx Int))
  (say '(declare-const ry Int))
  (say '(declare-const cy Int))
  (say '(declare-const rv Int))
  (say '(declare-const cv Int))
  (say '(assert (= cx (* cv t))))
  (say '(assert (= ry (* rv t))))
  (for-each
    (lambda (v)
      (say `(assert (< 0 ,v)))
      (say `(assert (<= ,v 10))))
    '(cv rv))
  (for-each
    (lambda (x)
      (say `(assert (<= 0 ,x)))
      (say `(assert (<= ,x 400))))
    '(rx cx ry cy))
  (say `(assert (<= 0 (- cy ry))))
  (say `(assert (>= 100 (- cy ry)))))


(define (run-world constraints scene)
  (match (process "z3 -in")
    [(list p-in p-out _ p-err p-fun)
     (define (say msg)
       ;(printf "~a\n" msg)
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
     (define (get-current-model t m ms)
       ;(println m)
       (say '(push))
       (say `(assert (= t ,t)))
       (for-each
         (lambda (kv)
           (say `(assert-soft (= ,(car kv) ,(cdr kv)) :weight 1)))
         m)
       (for-each
         (lambda (other-m)
           (say `(assert
                  ,(cons 'or
                        (map
                         (lambda (kv)
                           `(not (<= 50 (abs (- ,(car kv) ,(cdr kv))))))
                         other-m)))))
         ms)
       (let* ((ok (check-sat))
              (m (if ok (get-model) m)))
         (say '(pop))
         (list ok m)))
     (define (get-current-models t ms)
       (foldr (lambda (m ms)
                (match (get-current-model t m ms)
                  [(list ok m)
                   (if ok (cons m ms) ms)])) '() ms))
     (define (spur-new-models t n ms)
       (if (= n 0)
           ms
           (match (get-current-model t '() ms)
             [(list ok m)
              (if ok (spur-new-models t (- n 1) (cons m ms)) ms)])))
     
     (constraints say)
     (big-bang (cons 0 (spur-new-models 0 10 '()))
               (on-tick (lambda (tm)
                          (let ((t (car tm))
                                (ms (cdr tm)))
                            (cons (+ t 1) (get-current-models t ms)))))
               (stop-when (lambda (tm) (null? (cdr tm))))
               (to-draw (lambda (tm) (foldr scene (empty-scene 400 400 "white") (cdr tm)))))

     (close-input-port p-in)
     (close-output-port p-out)
     (close-input-port p-err)]))


(run-world ex2-constraints ex2-scene)
