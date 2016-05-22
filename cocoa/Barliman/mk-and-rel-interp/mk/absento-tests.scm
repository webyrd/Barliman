; Tests commented out are for general absento; this implementation only supports
; absento where the first argument is a ground atom.

;(test "test 0"
  ;(run* (q) (absento q q))
  ;'())

;(test "test 1"
  ;(run* (q)
    ;(fresh (a b c)
      ;(== a b)
      ;(absento b c)
      ;(== c b)
      ;(== `(,a ,b ,c) q)))
  ;'())

;(test "test 2"
  ;(run* (q)
    ;(fresh (a)
      ;(absento q a)
      ;(absento `((,q ,q) 3 (,q ,q)) `(,a 3 ,a))))
  ;'(_.0))

;(test "test 3"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento q a)
      ;(absento `(3 ,a) `(,b ,a))
      ;(== 3 b)))
  ;'())

;(test "test 4"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento q a)
      ;(absento `(3 ,a) `(,q ,a))
      ;(== 3 b)))
  ;'((_.0 (=/= ((_.0 3))))))

;(test "test 5"
  ;(run* (q)
    ;(fresh (a b)
      ;(numbero a)
      ;(numbero b)
      ;(absento '(3 3) `(,a ,b))
      ;(=/= a b)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (=/= ((_.0 _.1))) (num _.0 _.1))))

;(test "test 6"
  ;(run* (q) (fresh (a) (absento q a) (== q a)))
  ;'())

;(test "test 7"
  ;(run* (q)
    ;(fresh (a b c)
      ;(absento '(3 . 4) c)
      ;(== `(,a . ,b) c)
      ;(== q `(,a . ,b))))
  ;'(((_.0 . _.1) (=/= ((_.0 3) (_.1 4))) (absento ((3 . 4) _.0) ((3 . 4) _.1)))))

(test "test 8"
  (run* (q)
    (fresh (a b)
      (absento 5 a)
      (symbolo b)
      (== `(,q ,b) a)))
  '((_.0 (absento (5 _.0)))))

(test "test 9"
  (run* (q)
    (fresh (a b)
      (absento 5 a)
      (== `(,q ,b) a)))
  '((_.0 (absento (5 _.0)))))

;(test "test 10"
  ;(run* (q) (fresh (a) (absento `(3 . ,a) q) (absento q `(3 . ,a))))
  ;'((_.0 (=/= ((_.0 3))))))

;(test "test 11"
  ;(run* (q)
    ;(fresh (a b c d e f)
      ;(absento `(,a . ,b) q)
      ;(absento q `(,a . ,b))
      ;(== `(,c . ,d) a)
      ;(== `(3 . ,e) c)
      ;(== `(,f . 4) d)))
  ;'((_.0 (=/= ((_.0 3)) ((_.0 4))))))

;(test "test 12"
  ;(run* (q)
    ;(fresh (a b c)
      ;(absento `(,3 . ,a) `(,b . ,c))
      ;(numbero b)
      ;(== `(,a ,b ,c) q)))
  ;'(((_.0 _.1 _.2) (=/= ((_.0 _.2) (_.1 3))) (num _.1) (absento ((3 . _.0) _.2)))))

;(test "test 13"
  ;(run* (q)
    ;(fresh (a b c)
      ;(== `(,a . ,b) q)
      ;(absento '(3 . 4) q)
      ;(numbero a)
      ;(numbero b)))
  ;'(((_.0 . _.1) (=/= ((_.0 3) (_.1 4))) (num _.0 _.1))))

;(test "test 14"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento '(3 . 4) `(,a . ,b))
      ;(== `(,a . ,b) q)))
  ;'(((_.0 . _.1) (=/= ((_.0 3) (_.1 4))) (absento ((3 . 4) _.0) ((3 . 4) _.1)))))

;(test "test 15"
  ;(run* (q)
    ;(absento q `(3 . (4 . 5))))
  ;'((_.0 (=/= ((_.0 3))
              ;((_.0 4))
              ;((_.0 5))
              ;((_.0 (3 . (4 . 5))))
              ;((_.0 (4 . 5)))))))

;(test "test 16"
  ;(run* (q)
    ;(fresh (a b x)
      ;(absento a b)
      ;(symbolo a)
      ;(numbero x)
      ;(== x b)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (num _.1) (sym _.0))))

(test "test 19"
  (run* (q) (absento 5 q) (absento 5 q))
  '((_.0 (absento (5 _.0)))))

(test "test 20"
  (run* (q) (absento 5 q) (absento 6 q))
  '((_.0 (absento (5 _.0) (6 _.0)))))

(test "test 21"
  (run* (q) (absento 5 q) (symbolo q))
  '((_.0 (sym _.0))))

(test "test 22"
  (run* (q) (numbero q) (absento 'tag q))
  '((_.0 (num _.0))))

(test "test 23"
  (run* (q) (absento 'tag q) (numbero q))
  '((_.0 (num _.0))))

(test "test 24"
  (run* (q) (== 5 q) (absento 5 q))
  '())

(test "test 25"
  (run* (q) (== q `(5 6)) (absento 5 q))
  '())

(test "test 25b"
  (run* (q) (absento 5 q) (== q `(5 6)))
  '())

(test "test 26"
  (run* (q) (absento 5 q) (== 5 q))
  '())

(test "test 27"
  (run* (q) (absento 'tag1 q) (absento 'tag2 q))
  '((_.0 (absento (tag1 _.0) (tag2 _.0)))))

(test "test 28"
  (run* (q) (absento 'tag q) (numbero q))
  '((_.0 (num _.0))))

;(test "test 29"
   ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)
      ;(symbolo a)
      ;(numbero b)))
  ;'(((_.0 _.1) (num _.1) (sym _.0))))

;(test "test 30"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento b a)
      ;(absento a b)
      ;(== `(,a ,b) q)
      ;(symbolo a)
      ;(symbolo b)))
  ;'(((_.0 _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

;(test "test 31"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (absento (_.0 _.1) (_.1 _.0)))))

(test "test 32"
  (run* (q)
    (fresh (a b)
      (absento 5 a)
      (absento 5 b)
      (== `(,a . ,b) q)))
  '(((_.0 . _.1) (absento (5 _.0) (5 _.1)))))

(test "test 33"
  (run* (q)
    (fresh (a b c)
      (== `(,a ,b) c)
      (== `(,c ,c) q)
      (symbolo b)
      (numbero c)))
  '())

(test "test 34"
  (run* (q) (absento 'tag q) (symbolo q))
  '((_.0 (=/= ((_.0 tag))) (sym _.0))))

(test "test 35"
  (run* (q) (absento 5 q) (numbero q))
  '((_.0 (=/= ((_.0 5))) (num _.0))))

;(test "test 36"
  ;(run* (q)
    ;(fresh (a)
      ;(== 5 a) (absento a q)))
  ;'((_.0 (absento (5 _.0)))))

;(test "test 37"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)
      ;(symbolo a)
      ;(symbolo b)))
  ;'(((_.0 _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "test 38"
  (run* (q) (absento '() q))
  '((_.0 (absento (() _.0)))))

;(test "test 39"
  ;(run* (q) (absento `(3 4) q))
  ;'((_.0 (absento ((3 4) _.0)))))

(test "test 40"
  (run* (q)
    (fresh (d a c)
      (== `(3 . ,d) q)
      (=/= `(,c . ,a) q)
      (== '(3 . 4) d)))
  '((3 3 . 4)))

(test "test 41"
  (run* (q)
    (fresh (a)
      (== `(,a . ,a) q)))
  '((_.0 . _.0)))

;(test "test 42"
  ;(run* (q)
    ;(fresh (a b)
      ;(==  `((3 4) (5 6)) q)
      ;(absento `(3 4) q)))
  ;'())

;(test "test 43"
  ;(run* (q) (absento q 3))
  ;'((_.0 (=/= ((_.0 3))))))

;(test "test 44"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (absento (_.0 _.1) (_.1 _.0)))))

;(test "test 45"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento `(,a . ,b) q)
      ;(== q `(3 . (,b . ,b)))))
  ;'((3 _.0 . _.0)))

;(test "test 45b"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento `(,a . ,b) q)
      ;(== q `(,a 3 . (,b . ,b)))))
  ;'(((_.0 3 _.1 . _.1) (=/= ((_.0 _.1))))))

;(test "test 46"
  ;(run* (q)
    ;(fresh (a)
      ;(absento a q)
      ;(absento q a)))
  ;'(_.0))

;(test "test 47"
  ;(run* (q)
    ;(fresh (a)
      ;(absento `(,a . 3) q)))
  ;'(_.0)) 

;(test "test 48"
  ;(run* (q)
    ;(fresh (a)
      ;(absento `(,a . 3) q)))
  ;'(_.0))

;(test "test 49"
  ;(run* (q)
    ;(fresh (a b c d e)
      ;(absento `((3 4 ,a) (4 ,b) ((,c)) ,d ,e) q)))
  ;'(_.0))

;(test "test 50"
  ;(run* (q)
    ;(fresh (a)
      ;(absento a q)
      ;(== 5 a)))
  ;'((_.0 (absento (5 _.0)))))

;(test "test 51"
  ;(run* (q)
    ;(fresh (a b c d)
      ;(== a 5)
      ;(== a b)
      ;(== b c)
      ;(absento d q)
      ;(== c d)))
  ;'((_.0 (absento (5 _.0)))))

;(test "test 52"
  ;(run* (q)
    ;(fresh (a b c d)
      ;(== a b)
      ;(== b c)
      ;(absento a q)
      ;(== c d)
      ;(== d 5)))
  ;'((_.0 (absento (5 _.0)))))

;(test "test 53"
  ;(run* (q)
    ;(fresh (t1 t2 a)
      ;(== `(,a . 3) t1)
      ;(== `(,a . (4 . 3)) t2)
      ;(== `(,t1 ,t2) q)
      ;(absento t1 t2)))
  ;'((((_.0 . 3) (_.0 4 . 3)) (=/= ((_.0 4))))))

;(test "test 54"
  ;(run* (q)
    ;(fresh (a)
      ;(== `(,a . 3) q)
      ;(absento q `(,a . (4 . 3)))))
  ;'(((_.0 . 3) (=/= ((_.0 4))))))

;(test "test 55"
  ;(run* (q)
    ;(fresh (a d c)
      ;(== '(3 . 4) d)
      ;(absento `(3 . 4) q)
      ;(== `(3 . ,d) q)))
  ;'())

;(test "test 56"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)
      ;(symbolo a)
      ;(numbero b)))
  ;'(((_.0 _.1) (num _.1) (sym _.0))))


;(test "test 57"
  ;(run* (q)
    ;(numbero q)
    ;(absento q 3))
  ;'((_.0 (=/= ((_.0 3))) (num _.0))))

;(test "test 58"
  ;(run* (q)
    ;(fresh (a)
      ;(== `(,a . 3) q)
      ;(absento q `(,a . (4 . (,a . 3))))))
  ;'())

;(test "test 59"
  ;(run* (q) 
    ;(fresh (a) 
      ;(== `(,a . 3) q)
      ;(absento q `(,a . ((,a . 3) . (,a . 4))))))
  ;'())

;(test "test 60"
  ;(run* (q)
    ;(fresh (a d c)
      ;(== `(3 . ,d) q)
      ;(== '(3 . 4) d)
      ;(absento `(3 . 4) q)))
  ;'())

;(test "test 61"
  ;(run* (q)
    ;(fresh (a b c)
      ;(symbolo b)
      ;(absento `(,3 . ,a) `(,b . ,c))
      ;(== `(,a ,b ,c) q)))
  ;'(((_.0 _.1 _.2) (sym _.1) (absento ((3 . _.0) _.2)))))

;(test "test 62"
  ;(run* (q) (fresh (a b c) (absento a b) (absento b c) (absento c q) (symbolo a)))
  ;'(_.0))

(test "test 63"
  (run* (q) (fresh (a b c) (=/= a b) (=/= b c) (=/= c q) (symbolo a)))
  '(_.0))

(test "test 64"
  (run* (q) (symbolo q) (== 'tag q))
  '(tag))

;(test "test 65"
  ;(run* (q) (fresh (b) (absento '(3 4) `(,q ,b))))
  ;'((_.0 (absento ((3 4) _.0)))))

(test "test 66"
  (run* (q) (absento 6 5))
  '(_.0))

(test "test 67"
  (run* (q)
    (fresh (a b)
      (=/= a b)
      (symbolo a)
      (numbero b)
      (== `(,a ,b) q)))
  '(((_.0 _.1) (num _.1) (sym _.0))))

(test "test 68"
  (run* (q)
    (fresh (a b c d)
      (=/= `(,a ,b) `(,c ,d))
      (symbolo a)
      (numbero c)
      (symbolo b)
      (numbero c)
      (== `(,a ,b ,c ,d) q)))
  '(((_.0 _.1 _.2 _.3) (num _.2) (sym _.0 _.1))))

(test "test 69"
  (run* (q)
    (fresh (a b)
      (=/= `(,a . 3) `(,b . 3))
      (symbolo a)
      (numbero b)
      (== `(,a ,b) q)))
   '(((_.0 _.1) (num _.1) (sym _.0))))

;(test "test 70"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)
      ;(symbolo a)
      ;(numbero b)))
  ;'(((_.0 _.1) (num _.1) (sym _.0))))

;(test "test 70b"
  ;(run* (q)
    ;(fresh (a b)
      ;(symbolo a)
      ;(numbero b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (num _.1) (sym _.0))))

;(test "test 71"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)
      ;(symbolo a)
      ;(symbolo b)))
  ;'(((_.0 _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

;(test "test 72"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(absento b a)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (absento (_.0 _.1) (_.1 _.0)))))

;(test "test 73"
  ;(run* (q)
    ;(fresh (a b)
      ;(== `(,a ,b) q)
      ;(absento b a)
      ;(absento a b)
      ;(== a '(1 . 2))))
  ;'((((1 . 2) _.0)
   ;(=/= ((_.0 1)) ((_.0 2)))
   ;(absento ((1 . 2) _.0)))))

;(test "test 74"
  ;(run* (q)
    ;(fresh (a b c)
      ;(absento a q)
      ;(absento q a)
      ;(== `(,b . ,c) a)
      ;(== '(1 . 2) b)
      ;(== '(3 . 4) c)))
  ;'((_.0 (=/= ((_.0 1)) ((_.0 2)) ((_.0 3)) ((_.0 4))
           ;((_.0 (1 . 2))) ((_.0 (3 . 4))))
      ;(absento (((1 . 2) 3 . 4) _.0)))))

;(test "test 75"
   ;(run* (q)
     ;(fresh (a b c d e f g)
       ;(absento a q)
       ;(absento q a)
       ;(== `(,b . ,c) a)
       ;(== `(,d . ,e) b)
       ;(== `(,f . ,g) c)
       ;(== '(1 . 2) d)
       ;(== '(3 . 4) e)
       ;(== '(5 . 6) f)
       ;(== '(7 . 8) g)))
   ;'((_.0 (=/= ((_.0 ((1 . 2) 3 . 4)))
               ;((_.0 ((5 . 6) 7 . 8)))
               ;((_.0 1))
               ;((_.0 2))
               ;((_.0 3))
               ;((_.0 4))
               ;((_.0 5))
               ;((_.0 6))
               ;((_.0 7))
               ;((_.0 8))
               ;((_.0 (1 . 2)))
               ;((_.0 (3 . 4)))
               ;((_.0 (5 . 6)))
               ;((_.0 (7 . 8))))
          ;(absento ((((1 . 2) 3 . 4) (5 . 6) 7 . 8) _.0)))))

;(test "test 76"
   ;(run* (q)
     ;(absento 3 q)
     ;(absento '(3 4) q))
   ;'((_.0 (absento (3 _.0)))))

;(test "test 77"
  ;(run* (q)
     ;(fresh (x a b)
       ;(== x `(,a ,b))
       ;(absento '(3 4) x)
       ;(absento 3 a)
       ;(absento 4 b)
       ;(== q `(,a 2))))
  ;'(((_.0 2) (absento (3 _.0)))))

;(test "test 78"
  ;(run* (q)
    ;(fresh (d)
      ;(== `(3 . ,d) q)
      ;(absento `(3 . 4) q)
      ;(== '(3 . 4) d)))
  ;'())

;(test "test 79"
  ;(run* (q)
    ;(fresh (d)
      ;(absento `(3 . 4) q)
      ;(== `(3 . ,d) q)
      ;(== '(3 . 4) d)))
  ;'())

;(test "test 80"
  ;(run* (q)
    ;(fresh (d a c)
      ;(== `(3 . ,d) q)
      ;(absento `(3 . ,a) q)
      ;(== c d)
      ;(== `(3 . ,a) c)))
  ;'())

;(test "test 81"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento `(3 . ,a) `(,b . 4))
      ;(== `(,a . ,b) q)))
  ;'(((_.0 . _.1) (=/= ((_.0 4) (_.1 3))) (absento ((3 . _.0) _.1)))))

;(test "test 82"
  ;(run* (q)
    ;(fresh (d)
      ;(== `(3 . ,d) q)
      ;(absento `(3 . 4) q)))
  ;'(((3 . _.0) (=/= ((_.0 4))) (absento ((3 . 4) _.0)))))

;(test "test 83"
  ;(run* (q)
    ;(fresh (d)
      ;(== `(3 . ,d) q)
      ;(== '(3 . 4) d))
      ;(absento `(3 . 4) q))
  ;'())

;(test "test 84"
  ;(run* (q) 
    ;(fresh (a b c d) 
      ;(=/= `(,a . ,b) `(,c . ,d))
      ;(absento a c)
      ;(== `(,a ,b ,c ,d) q)))
  ;'(((_.0 _.1 _.2 _.3) (absento (_.0 _.2)))))

;(test "test 84 b"
  ;(run* (q)
    ;(fresh (a b c d) 
      ;(=/= `(,a . ,b) `(,c . ,d)) 
      ;(absento c a) 
      ;(== `(,a ,b ,c ,d) q)))
  ;'(((_.0 _.1 _.2 _.3) (absento (_.2 _.0)))))

;(test "test 85 a"
  ;(run* (q)
    ;(fresh (a b)
      ;(=/= a b)
      ;(absento a b)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (absento (_.0 _.1)))))

;(test "test 85 b"
  ;(run* (q)
    ;(fresh (a b)
      ;(absento a b)
      ;(=/= a b)
      ;(== `(,a ,b) q)))
  ;'(((_.0 _.1) (absento (_.0 _.1)))))
