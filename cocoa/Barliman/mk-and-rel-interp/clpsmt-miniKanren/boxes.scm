(load "mk.scm")
(load "z3-driver.scm")
(load "test-check.scm")

(set! defer-smt-checks #t)

(define (is-boxo b x y w h)
  (fresh ()
    (z/assert `(<= 0 ,x))
    (z/assert `(<= 0 ,y))
    (z/assert `(<= 0 ,w))
    (z/assert `(<= 0 ,h))
    (== b `(box ,x ,y ,w ,h))))

(define (make-boxo w h b)
  (fresh (x y)
    (is-boxo b x y w h)))

(define (withino ax aw bx bw)
  (fresh ()
    (z/assert `(>= ,ax ,bx))
    (z/assert `(<= (+ ,ax ,aw) (+ ,bx ,bw)))))

(define (not-withino ax aw bx bw)
  (fresh ()
    (conde
      ((z/assert `(< ,ax ,bx)))
      ((z/assert `(> (+ ,ax ,aw) (+ ,bx ,bw)))))))

(define (contains-boxo a b)
  (fresh (ax ay aw ah bx by bw bh)
    (is-boxo a ax ay aw ah)
    (is-boxo b bx by bw bh)
    (withino ax aw bx bw)
    (withino ay ah by bh)))

(define (overlaps-boxo a b)
  (fresh (ax ay aw ah bx by bw bh)
    (is-boxo a ax ay aw ah)
    (is-boxo b bx by bw bh)
    (z/assert `(not (= ,ax ,bx)))
    (z/assert `(not (= ,ay ,by)))
    (conde
      ((withino ax aw bx bw)
       (not-withino ay ah by bh))
      ((withino ay ah by bh)
       (not-withino ax aw bx bw)))))

(define (separated-boxo a b)
  (fresh (ax ay aw ah bx by bw bh)
    (is-boxo a ax ay aw ah)
    (is-boxo b bx by bw bh)
    (not-withino ax aw bx bw)
    (not-withino ay ah by bh)))

(test "impossible-boxes"
  (run* (q)
    (fresh (a b c)
      (== q (list a b c))
      (make-boxo 2 2 a)
      (make-boxo 3 3 b)
      (contains-boxo b a)))
  '())

(test "possible-boxes"
  (run 1 (q)
    (fresh (a b c)
      (== q (list a b c))
      (make-boxo 2 2 a)
      (make-boxo 3 3 b)
      (make-boxo 4 4 c)
      (contains-boxo a b)
      (overlaps-boxo b c)))
  '(((box 1 0 2 2) (box 1 0 3 3) (box 0 1 4 4))))
