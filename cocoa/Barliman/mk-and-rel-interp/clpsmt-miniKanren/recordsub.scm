(define typ
  (lambda (ty)
    (conde
      ((fresh (ty1 ty2)
         (== `(arr ,ty1 ,ty2) ty)
         (typ ty1)
         (typ ty2)))
      ((fresh (s env)
         (z/set s)
         (== `(rcd ,s ,env) ty)
         (typ-rcd s env))))))

(define typ-rcd
  (lambda (s env)
    (conde
      ((z/assert `(= ,s ,∅)))
      ((fresh (l ft sr er)
         (z/set sr)
         (z/assert `(= ,s ,(set sr l)))
         (!ino l sr)
         (numbero l)
         (== (cons (cons l ft) er) env)
         (typ ft)
         (typ-rcd sr er))))))

'(
  (run 10 (q)
    (fresh (s env)
      (== q (list s env))
      (z/set s)
      (typ-rcd s env)))
 )

(define sub
  (lambda (ty1 ty2)
    (conde
      ((== ty1 ty2))
      ((=/= ty1 ty2)
       (conde
         ((fresh (tya1 tyb1 tya2 tyb2)
            (== `(arr ,tya1 ,tyb1) ty1)
            (== `(arr ,tya2 ,tyb2) ty2)
            (sub tya2 tya1)
            (sub tyb1 tyb2)))
         ((fresh (s1 s2 e1 e2)
            (z/set s1)
            (z/set s2)
            (== `(rcd ,s1 ,e1) ty1)
            (== `(rcd ,s2 ,e2) ty2)
            (sub-rcd s1 e1 s2 e2))))))))

(define sub-rcd
  (lambda (s1 e1 s2 e2)
    (subseto s1 s2)
    ;; TODO
    ))

'(
   (run 10 (q)
     (fresh (ty1 ty2)
       (=/= ty1 ty2)
       (=/= ty2 `(rcd ,∅ ()))
       (== q `(,ty1 ,ty2))
       (sub ty1 ty2)))
 )

;; more to do...
