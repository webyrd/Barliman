(define ∅L '(as emptyset (Set L)))

(define L
  (lambda ()
    (z/
     `(declare-datatypes
       ((L 0)) 
       (((tru) (fls) (zero) (succ (s1 L)) (pred (p1 L)) (iszero (z1 L))))))))

(define fresh/L
  (lambda (t xs)
    (if (null? xs) succeed (fresh () (z/ `(declare-const ,(car xs) ,t)) (freshL t (cdr xs))))))

(define map-seto
  (lambda (fo s out)
    (conde
     ((z/== s ∅L)
      (z/== out ∅L))
     ((fresh (se sr oe or)
        (fresh/L '(Set L) (list sr or))
        (fresh/L 'L (list se oe))
        (z/== s (set sr se))
        (!ino se sr)
        (z/== out (set or oe))
        (!ino oe or)
        (fo se oe)
        (map-seto fo sr or))))))

(define S
  (lambda (i s)
    (conde
     ((z/== i 0) (z/== s ∅L))
     ((fresh (i-1 S-1 S-11 S-111 S-succ S-pred S-iszero s1 s2)
        (freshL '(Set L) (list S-1 S-succ S-pred S-iszero s1 s2))
        (z/== i `(+ 1 ,i-1))
        (S i-1 S-1)
        (map-seto (lambda (e o) (z/== o `(succ ,e))) S-1 S-succ)
        (map-seto (lambda (e o) (z/== o `(pred ,e))) S-1 S-pred)
        (map-seto (lambda (e o) (z/== o `(iszero ,e))) S-1 S-iszero)
        (uniono (set ∅L 'tru 'fls 'zero) S-succ s1)
        (uniono s1 S-pred s2)
        (uniono s2 S-iszero s))))))

(test "S0"
 (run* (q) (L) (freshL '(Set L) (list q))
   (S 0 q))
 `(,∅L))

(test "S1"
 (run* (q) (L) (freshL '(Set L) (list q))
       (S 1 q))
 '((union
   [union (singleton tru) (singleton fls)]
   [singleton zero])))

(ignore "S2"
  (run 1 (q) (L) (freshL '(Set L) (list q))
    (S 2 q))
  'works-but-not-pretty)
