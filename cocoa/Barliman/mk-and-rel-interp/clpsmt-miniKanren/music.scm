(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

(define perfect-consonant '(0 5 7))
(define consonant '(0 3 4 5 7 8 9))
(define imperfect-consonant '(3 4 8 9))

(define harmony
  '((1 (3 6 2 4 5))
    (2 (5 7))
    (3 (6))
    (4 (5 7))
    (5 (1))
    (6 (2 4))
    (7 (1))))

(define (interval-ino ds note harmony)
  (fresh (d dr)
    (== (cons d dr) ds)
    (conde
      ((z/assert `(= (- ,note ,harmony) ,d)))
      ((z/assert `(not (= (- ,note ,harmony) ,d)))
       (interval-ino dr note harmony)))))

(define (ino xs x)
  (fresh (y ys)
    (== (cons y ys) xs)
    (conde
      ((z/assert `(= ,x ,y)))
      ((z/assert `(not (= ,x ,y)))
       (ino ys x)))))

(define (nexto harmony prev-harmony cur-harmony)
  (fresh (p hs cs)
    (== (cons `(,p ,cs) hs) harmony)
    (conde
      ((z/assert `(= ,p ,prev-harmony))
       (ino cs cur-harmony))
      ((z/assert `(not (= ,p ,prev-harmony)))
       (nexto hs prev-harmony cur-harmony)))))

(define (zico measure phrase position prev-note cur-note prev-harmony cur-harmony)
  (fresh ()
    (nexto harmony prev-harmony cur-harmony)
    (conde
      ((z/assert `(= 0 (mod ,position ,measure)))
       (== cur-harmony 1)
       (interval-ino perfect-consonant cur-note cur-harmony))
      ((z/assert `(not (= 0 (mod ,position ,measure))))
       (interval-ino imperfect-consonant cur-note cur-harmony)))))

(define (musico measure phrase position prev-note prev-harmony m)
  (conde
    ((z/assert `(= ,position ,(* measure phrase)))
     (== m '()))
    ((z/assert `(< ,position ,(* measure phrase)))
     (fresh (position+1 cur-note cur-harmony rest-m)
       (== m (cons (list cur-note cur-harmony) rest-m))
       (z/assert `(= ,position+1 (+ 1 ,position)))
       (zico measure phrase position prev-note cur-note prev-harmony cur-harmony)
       (musico measure phrase position+1 cur-note cur-harmony rest-m)))))

(test "1"
  (run 1 (m)
    (musico 1 1 0 5 5 m))
  '(((1 1))))

(test "5" ;; slow
  (run 1 (m)
    (musico 5 1 0 5 5 m))
  '(((1 1) (6 3) (9 6) (5 2) (8 5))))

(test "4-2" ;; very slow
  (run 1 (m)
    (musico 4 2 0 5 5 m))
  '(((1 1) (9 6) (5 2) (8 5) (1 1) (6 3) (9 6) (5 2))))
