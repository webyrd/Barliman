;; A terminating 0cfa abstract interpreter via ADI caching
;; based on work by David Darais
;; For ADI, see http://david.darais.com/assets/papers/abstracting-definitional-interpreters/adi.pdf

;;; 'icache' stands for "inner cache"
;;;
;;; 'ocache' stands for "outer cache"

(define (conso x xs r)
  (== (cons x xs) r))

(define (mapo r xs f)
  (conde
    [(== xs '()) (== r '())]
    [(fresh [y ys z zs]
       (conso y ys xs)
       (conso z zs r)
       (f y z)
       (mapo zs ys f))]))

(define (foldlo f x xs r)
  (conde
    [(== xs '()) (== x r)]
    [(fresh [y ys z]
       (conso y ys xs)
       (f x y z)
       (foldlo f z ys r))]))

(define (lookupo x bs out)
  (conde
    ;; #f is never bound as a value in the store,
    ;; we don't have booleans.
    [(== bs '()) (== out #f)]
    [(fresh [b br y v]
       (conso b br bs)
       (== `(,y ,v) b)
       (conde
         [(== y x) (== v out)]
         [(=/= y x) (lookupo x br out)]))]))

(define (ino x xs)
  (fresh [y ys]
    (conso y ys xs)
    (conde
      [(== y x)]
      [(=/= y x)
       (ino x ys)])))

(define (not-ino x xs)
  (conde
    [(== xs '())]
    [(fresh [y ys]
       (conso y ys xs)
       (=/= y x)
       (not-ino x ys))]))

(define (includo a b)
  (conde
    [(== a '())]
    [(fresh [a1 ar]
       (conso a1 ar a)
       (ino a1 b)
       (includo ar b))]))

(define (set-equivo a b)
  (fresh ()
    (includo a b)
    (includo b a)))

(define (excludo a b)
  (fresh [a1 ar]
    (conso a1 ar a)
    (conde
      [(not-ino a1 b)]
      [(ino a1 b) (excludo ar b)])))

(define (not-set-equivo a b)
  (conde
    [(excludo a b)]
    [(includo a b) (excludo b a)]))

(define (set-uniono a b c)
  (conde
    [(== a '()) (== b c)]
    [(fresh [xa ra]
       (conso xa ra a)
       (conde
         [(ino xa b)
          (set-uniono ra b c)]
         [(not-ino xa b)
          (fresh [rc]
            (conso xa rc c)
            (set-uniono ra b rc))]))]))

(define (set-unionso xs out)
  (foldlo set-uniono
          '()
          xs
          out))

(define (set-unionso2 xs out)
  (foldlo (lambda (a b c)
            (set-uniono a `(,b) c))
          '()
          xs
          out))

(define (injo i a)
  (conde
    [(z/assert `(< ,i 0)) (== a 'neg)]
    [(== i 0) (== a 'zer)]
    [(z/assert `(> ,i 0)) (== a 'pos)]))

(define (combo f u os1 s1 s2 s3)
  (conde
    [(== '() s2)
     (== '() s3)]
    [(== s1 '())
     (fresh [a2 r2]
       (conso a2 r2 s2)
       (combo f u os1 os1 r2 s3))]
    [(fresh [a1 r1 a2 r2 sa sm]
       (conso a1 r1 s1)
       (conso a2 r2 s2)
       (f a1 a2 sa)
       (combo f u os1 r1 s2 sm)
       (u sa sm s3))]))

(define (combino f s1 s2 s3)
  (combo f set-uniono s1 s1 s2 s3))

(define (plusdo a b s)
  (conde
    [(== a 'neg) (== b 'neg) (== s `(neg))]
    [(== a 'neg) (== b 'zer) (== s `(neg))]
    [(== a 'neg) (== b 'pos) (== s `(neg zer pos))]
    [(== a 'zer) (== b 'neg) (== s `(neg))]
    [(== a 'zer) (== b 'zer) (== s `(zer))]
    [(== a 'zer) (== b 'pos) (== s `(pos))]
    [(== a 'pos) (== b 'neg) (== s `(neg zer pos))]
    [(== a 'pos) (== b 'zer) (== s `(pos))]
    [(== a 'pos) (== b 'pos) (== s `(pos))]))

(define (plusso s1 s2 s3)
  (combino plusdo s1 s2 s3))

(define (timeso a b c)
  (conde
    [(== a 'zer) (== c 'zer)]
    [(=/= a 'zer) (== b 'zer) (== c 'zer)]
    [(== a 'neg) (== b 'neg) (== c 'pos)]
    [(== a 'neg) (== b 'pos) (== c 'neg)]
    [(== a 'pos) (== b 'neg) (== c 'neg)]
    [(== a 'pos) (== b 'pos) (== c 'pos)]))

(define (timesdo a b s)
  (fresh [c]
    (timeso a b c)
    (== s `(,c))))

(define (timesso s1 s2 s3)
  (combino timesdo s1 s2 s3))

(define (lubo a1 a2 a3)
  (fresh [is1 cs1 is2 cs2 is3 cs3]
    (== `(aval ,is1 ,cs1) a1)
    (== `(aval ,is2 ,cs2) a2)
    (== `(aval ,is3 ,cs3) a3)
    (set-uniono is1 is2 is3)
    (set-uniono cs1 cs2 cs3)))

(define (replaceo x vo bs out)
  (fresh [b br y v]
    (conso b br bs)
    (== b `(,y ,v))
    (conde
      [(== y x) (conso `(,x ,vo) br out)]
      [(=/= y x)
       (fresh [r-out]
         (conso b r-out out)
         (replaceo x vo br r-out))])))

(define (add-lubo x v s out)
  (fresh [l]
    (lookupo x s l)
    (conde
      [(== l #f) (== out `((,x ,v) . ,s))]
      [(=/= l #f)
       (fresh [vo]
         (lubo v l vo)
         (replaceo x vo s out))])))

(define (add-uniono x v s out)
  (fresh [l]
    (lookupo x s l)
    (conde
      [(== l #f) (== out `((,x ,v) . ,s))]
      [(=/= l #f)
       (fresh [vo]
         (set-uniono v l vo)
         (replaceo x vo s out))])))

(define (adivalo-op2 opo e1 e2 s ocache icache out)
  (fresh [s1 r1]
    (adivalpto e1 s ocache icache s1)
    (mapo r1 s1
          (lambda (a1 o)
            (fresh [is1 cs1 sp icachep s2 r2]
              (== `((aval ,is1 ,cs1) ,sp ,icachep) a1)
              (adivalpto e2 sp ocache icachep s2)
              (mapo r2 s2
                    (lambda (a2 o2)
                      (fresh [is2 cs2 sp2 icachep2 is3]
                        (== `((aval ,is2 ,cs2) ,sp2 ,icachep2) a2)
                        (== `((aval ,is3 ()) ,sp2 ,icachep2) o2)
                        (opo is1 is2 is3))))
              (set-unionso2 r2 o))))
    (set-unionso r1 out)))

(define  (adivalo-condo c e r icachep ocache out)
  (fresh [s]
    (mapo s r
          (lambda (a o)
            (fresh [is cs sp]
              (== `((aval ,is ,cs) ,sp) a)
              (conde
                [(ino c is)
                 (adivalpto e sp ocache icachep o)]
                [(not-ino c is)
                 (== '() o)]))))
    (set-unionso s out)))

;; AVal is of the form (aval (Set AInt) (Set (Symbol,Symbol,Exp)))
;; AInt is one of neg zer pos
;; e: Exp
;; s: ASto (Map from symbol to AVal)
;; ocache: Cache (Map from (Exp,ASto) to (Set (AVal,ASto)))
;; icache: Cache
;; out: Set (AVal,ASto,Cache)
(define (adivalo e s ocache icache out)
  (conde
    [(fresh [x l]
       (== `(var ,x) e)
       (lookupo x s l)
       (conde
         [(== l #f) (== '() out)]
         [(=/= l #f) (== `((,l ,s ,icache)) out)]))]
    [(fresh [i a]
       (== `(int ,i) e)
       (== `(((aval (,a) ()) ,s ,icache)) out)
       (injo i a))]
    [(fresh [e1 e2 s1 s2]
       (== `(plus ,e1 ,e2) e)
       (adivalo-op2 plusso e1 e2 s ocache icache out))]
    [(fresh [e1 e2 s1 s2]
       (== `(times ,e1 ,e2) e)
       (adivalo-op2 timesso e1 e2 s ocache icache out))]
    [(fresh [x y e0]
       (== `(lam ,x ,y ,e0) e)
       (== `(((aval () ((,x ,y ,e0))) ,s ,icache)) out))]
    [(fresh [e1 e2]
       (== `(app ,e1 ,e2) e)
       (fresh [r1 s1]
         (adivalpto e1 s ocache icache s1)
         (mapo r1 s1
               (lambda (a1 o)
                 (fresh [r2 is1 cs1 sp icachep s2]
                   (== `((aval ,is1 ,cs1) ,sp ,icachep) a1)
                   (adivalpto e2 sp ocache icachep s2)
                   (mapo r2 s2
                         (lambda (a2 o2)
                           (fresh [r3 v2 sp2 icachep2 is3]
                             (== `(,v2 ,sp2 ,icachep2) a2)
                             (mapo r3 cs1
                                   (lambda (a3 o3)
                                     (fresh [x y e0 s0 sp2y]
                                       (== `(,x ,y ,e0) a3)
                                       (add-lubo y v2 sp2 sp2y)
                                       (add-lubo x `(aval () ((,x ,y ,e0))) sp2y s0)
                                       (adivalpto e0 s0 ocache icachep2 o3))))
                             (set-unionso r3 o2))))
                   (set-unionso r2 o))))
         (set-unionso r1 out)))]
    [(fresh [e1 e2 e3]
       (== `(if0 ,e1 ,e2 ,e3) e)
       (fresh [r1 icachep s1 s2 s3 si]
         (adivalpo e1 s ocache icache `(,r1 ,icachep))
         (adivalo-condo 'zer e2 r1 icachep ocache s1)
         (adivalo-condo 'pos e3 r1 icachep ocache s2)
         (adivalo-condo 'neg e3 r1 icachep ocache s3)
         (set-uniono s2 s3 si)
         (set-uniono s1 si out)))]))

;; out: (Set (AVal,ASto),Cache)
 (define (adivalwo e s ocache icache out)
   (fresh [res r c r-out c-out]
     (== `(,r-out ,c-out) out)
     (adivalo e s ocache icache res)
     (mapo r res (lambda (a o)
                   (fresh [x y z]
                     (== a `(,x ,y ,z))
                     (== o `(,x ,y)))))
     (mapo c res (lambda (a o)
                   (fresh [x y z]
                     (== a `(,x ,y ,z))
                     (== o z))))
     (set-unionso2 r r-out)
     (set-unionso c c-out)))

;; out: (Set (Aval,Asto),Cache)
(define (adivalpo e s ocache icache out)
  (fresh [r]
    (lookupo `(,e ,s) icache r)
    (conde
      [(=/= r #f) (== `(,r ,icache) out)]
      [(== r #f)
       (fresh [r0 o icachep r icachepp icacheppp]
         (== `(,r ,icacheppp) out)
         (conde
           [(== o #f) (== r0 '())]
           [(=/= o #f) (== r0 o)])
         (lookupo `(,e ,s) ocache o)
         (add-uniono `(,e ,s) r0 icache icachep)
         (adivalwo e s ocache icachep `(,r ,icachepp))
         (add-uniono `(,e ,s) r icachepp icacheppp))])))

;; out: Set (Aval,ASto,Cache)
(define (adivalpto e s ocache icache out)
  (fresh [r icachep]
    (adivalpo e s ocache icache `(,r ,icachep))
    (mapo out r (lambda (a o)
                  (fresh [vs sp]
                    (== `(,vs ,sp) a)
                    (== `(,vs ,sp ,icachep) o))))))

;; out: Cache
(define (lfppo e cache out)
  (fresh [r cachep]
    (adivalpo e '() cache '() `(,r ,cachep))
    (conde
      [(== out cache) (set-equivo cache cachep)]
      [(not-set-equivo cache cachep) (lfppo e cachep out)])))

;; out: Cache
(define (lfpo e out)
  (lfppo e '() out))

;; out: Set (AVal,Asto)
(define (analyzeo e out)
  (fresh [cache]
    (lfpo e cache)
    (lookupo `(,e ()) cache out)))

;; out: Cache
(define (iterpo n e cache out)
  (conde
    [(== n 0) (== out cache)]
    [(fresh [r cachep n-1]
       (z/assert `(= (+ 1 ,n-1) ,n))
       (adivalpo e '() cache '() `(,r ,cachep))
       (iterpo n-1 e cachep out))]))

;; out: Cache
(define (itero n e out)
  (iterpo n e '() out))
