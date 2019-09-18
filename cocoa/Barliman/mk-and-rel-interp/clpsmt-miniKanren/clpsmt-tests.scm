(define faco
  (lambda (n out)
    (conde ((z/assert `(= ,n 0))
            (z/assert `(= ,out 1)))
           ((z/assert `(> ,n 0))
            (fresh (n-1 r)
              (z/assert `(= (- ,n 1) ,n-1))
              (z/assert `(= (* ,n ,r) ,out))
              (faco n-1 r))))))

(test "faco-7"
  (run 7 (q)
    (fresh (n out)
      (faco n out)
      (== q `(,n ,out))))
  '((0 1) (1 1) (2 2) (3 6) (4 24) (5 120) (6 720)))

(test "faco-backwards-2"
  (run* (q)
    (faco q 2))
  '(2))

(test "faco-backwards-720"
  (run* (q)
    (faco q 720))
  '(6))

(load "full-interp.scm")

(test "evalo-1"
  (run* (q)
    (evalo '(+ 1 2) q))
  '(3))

(test "evalo-backwards-1"
  (run* (q)
    (evalo `(+ 0 ',q) 3))
  '(3))

(test "evalo-bop-1"
  (run* (q)
    (evalo `((lambda (n) (< n 0)) 0) q))
  '(#f))

(test "evalo-2"
  (run* (q)
    (evalo `(((lambda (f)
                (lambda (n) (if (< n 0) #f
                           (if (= n 0) 1
                               (* n (f (- n 1)))))))
              (lambda (x) 1))
             2)
           q))
  '(2))


(test "evalo-fac-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 6))
           q))
  '(720))

;; slowish
(test "evalo-fac-9"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac 9))
           q))
  '(362880))

(test "evalo-backwards-fac-6"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           720))
  '(6))

;; remember the quote!
(test "evalo-backwards-fac-quoted-6"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           720))
  '(6))


;; slowish
(test "evalo-backwards-fac-9"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ,q))
           362880))
  '(9))

;; remember the quote!
(test "evalo-backwards-fac-quoted-9"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (fac ',q))
           362880))
  '(9))


;; slowish
(test "evalo-fac-table"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           q))
  '((1 1 2 6)))

(test "evalo-fac-synthesis-hole-0"
  (run* (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) ',q
                                (* n (fac (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(1))

(test "evalo-fac-synthesis-hole-1"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (,q (- n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(fac))

;; takes a while
(test "evalo-fac-synthesis-hole-1-reversed-examples"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (,q (- n 1))))))))
              (list
               (fac 3)
               (fac 2)
               (fac 1)
               (fac 0)))
           '(6 2 1 1)))
  '(fac))

(test "evalo-fac-synthesis-hole-2"
  (run 1 (q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (- ,q 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(n))

(test "evalo-fac-synthesis-hole-3"
  (run 1 (q)
    (fresh (r s)
      (== (list r s) q)
      (evalo `(letrec ((fac
                        (lambda (n)
                          (if (< n 0) #f
                              (if (= n 0) 1
                                  (* n (fac (- ,r ,s))))))))
                (list
                 (fac 0)
                 (fac 1)
                 (fac 2)
                 (fac 3)))
             '(1 1 2 6))))
  '((n 1)))

;; slow, even with the 'symbolo' constraint on 'q'
(test "evalo-fac-synthesis-hole-4"
  (run 1 (q)
    (symbolo q)
    (evalo `(letrec ((fac
                      (lambda (n)
                        (if (< n 0) #f
                            (if (= n 0) 1
                                (* n (fac (,q n 1))))))))
              (list
               (fac 0)
               (fac 1)
               (fac 2)
               (fac 3)))
           '(1 1 2 6)))
  '(-))


(test "evalo-division-using-multiplication-0"
  (run* (q)
    (evalo `(* 3 ',q) 6))
  '(2))

(test "evalo-division-using-multiplication-1"
  (run* (q)
    (evalo `(* 4 ',q) 6))
  '())

(test "evalo-division-using-multiplication-2"
  (run* (q)
    (evalo `(* 3 ',q) 18))
  '(6))

(test "evalo-many-0"
  (run* (q)
    (fresh (x y)
      (evalo `(* ',x ',y) 6)
      (== q (list x y))))
  '((6 1) (1 6) (-1 -6) (-2 -3)
    (-3 -2) (-6 -1) (2 3) (3 2)))

(test "many-1"
  (run* (q)
    (fresh (x y)
      (evalo `(+ (* ',x ',y) (* ',x ',y)) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))

(test "many-2"
  (run* (q)
    (fresh (x y)
      (evalo `(* (* ',x ',y) 2) 6)
      (== q (list x y))))
  '((3 1) (1 3) (-1 -3) (-3 -1)))
