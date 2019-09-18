(load "mk.scm")
(load "cvc4-driver.scm")
(load "test-check.scm")

;; following https://barghouthi.github.io/2017/04/24/synthesis-primer/

(define (synthesize q exs)
  (fresh (a b)
    (let ((shape `(+ (* ,a x) ,b)))
      (fresh ()
        (z/ `(declare-const ,a Int))
        (z/ `(declare-const ,b Int))
        (z/ `(assert (forall ((x Int) (y Int))
                             (=> (or ,@(map (lambda (ex)
                                              `(and (= x ,(car ex))
                                                    (= y ,(cdr ex))))
                                            exs))
                                 (= y ,shape)))))
        z/purge
        (fresh (ax axb)
          (conde ((== a 1) (== ax 'x))
                 ((== a 0) (== ax 0))
                 ((=/= a 1) (=/= a 0) (== ax `(* ,a x))))
          (conde ((== ax 0) (== axb b))
                 ((=/= ax 0) (== b 0) (== axb ax))
                 ((=/= ax 0) (=/= b 0) (== axb `(+ ,ax ,b))))
          (== q `(lambda (x) ,axb)))))))


(test "syn-inc"
  (run* (q) (synthesize q '((1 . 2) (2 . 3))))
  '((lambda (x) (+ x 1))))

(test "syn-double"
  (run* (q) (synthesize q '((1 . 2) (2 . 4))))
  '((lambda (x) (* 2 x))))

(test "syn-const"
  (run* (q) (synthesize q '((1 . 2) (2 . 2))))
  '((lambda (x) 2)))

(test "syn-lin"
  (run* (q) (synthesize q '((1 . 3) (2 . 5))))
  '((lambda (x) (+ (* 2 x) 1))))

(test "syn-no"
  (run* (q) (synthesize q '((2 . 3) (3 . 2) (4 . 3))))
  '())
