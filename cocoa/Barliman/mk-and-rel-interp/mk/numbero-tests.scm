(test "numbero-1"
  (run* (q) (numbero q))
  '((_.0 (num _.0))))

(test "numbero-2"
  (run* (q) (numbero q) (== 5 q))
  '(5))

(test "numbero-3"
  (run* (q) (== 5 q) (numbero q))
  '(5))

(test "numbero-4"
  (run* (q) (== 'x q) (numbero q))
  '())

(test "numbero-5"
  (run* (q) (numbero q) (== 'x q))
  '())

(test "numbero-6"
  (run* (q) (numbero q) (== `(1 . 2) q))
  '())

(test "numbero-7"
  (run* (q) (== `(1 . 2) q) (numbero q))
  '())

(test "numbero-8"
  (run* (q) (fresh (x) (numbero x)))
  '(_.0))

(test "numbero-9"
  (run* (q) (fresh (x) (numbero x)))
  '(_.0))

(test "numbero-10"
  (run* (q) (fresh (x) (numbero x) (== x q)))
  '((_.0 (num _.0))))

(test "numbero-11"
  (run* (q) (fresh (x) (numbero q) (== x q) (numbero x)))
  '((_.0 (num _.0))))

(test "numbero-12"
  (run* (q) (fresh (x) (numbero q) (numbero x) (== x q)))
  '((_.0 (num _.0))))

(test "numbero-13"
  (run* (q) (fresh (x) (== x q) (numbero q) (numbero x)))
  '((_.0 (num _.0))))

(test "numbero-14-a"
  (run* (q) (fresh (x) (numbero q) (== 5 x)))
  '((_.0 (num _.0))))

(test "numbero-14-b"
  (run* (q) (fresh (x) (numbero q) (== 5 x) (== x q)))
  '(5))

(test "numbero-15"
  (run* (q) (fresh (x) (== q x) (numbero q) (== 'y x)))
  '())

(test "numbero-16-a"
  (run* (q) (numbero q) (=/= 'y q))
  '((_.0 (num _.0))))

(test "numbero-16-b"
  (run* (q) (=/= 'y q) (numbero q))
  '((_.0 (num _.0))))

(test "numbero-17"
  (run* (q) (numbero q) (=/= `(1 . 2) q))
  '((_.0 (num _.0))))

(test "numbero-18"
  (run* (q) (numbero q) (=/= 5 q))
  '((_.0 (=/= ((_.0 5))) (num _.0))))

(test "numbero-19"
  (run* (q)
    (fresh (x y)
      (numbero x)
      (numbero y)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (num _.0 _.1))))

(test "numbero-20"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (numbero y)))
  '(((_.0 _.1) (num _.0 _.1))))

(test "numbero-21"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (numbero x)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-22"
  (run* (q)
    (fresh (x y)
      (numbero x)
      (numbero x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-23"
  (run* (q)
    (fresh (x y)
      (numbero x)
      (== `(,x ,y) q)
      (numbero x)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-24-a"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (numbero w)
      (numbero z)))
  '(_.0))

(test "numbero-24-b"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (numbero w)
      (numbero z)
      (== `(,w ,x ,y ,z) q)))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.0 _.2) (_.1 _.3)))
     (num _.0 _.3))))

(test "numbero-24-c"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (numbero w)
      (numbero y)
      (== `(,w ,x ,y ,z) q)))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.0 _.2) (_.1 _.3)))
     (num _.0 _.2))))

(test "numbero-24-d"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (numbero w)
      (numbero y)
      (== w y)
      (== `(,w ,x ,y ,z) q)))
  '(((_.0 _.1 _.0 _.2)
     (=/= ((_.1 _.2)))
     (num _.0))))

(test "numbero-25"
  (run* (q)
    (fresh (w x)
      (=/= `(,w . ,x) `(a . b))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (=/= ((_.0 a) (_.1 b))))))

(test "numbero-26"
  (run* (q)
    (fresh (w x)
      (=/= `(,w . ,x) `(a . b))
      (numbero w)
      (== `(,w ,x) q)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-27"
  (run* (q)
    (fresh (w x)
      (numbero w)
      (=/= `(,w . ,x) `(a . b))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-28"
  (run* (q)
    (fresh (w x)
      (numbero w)
      (=/= `(a . b) `(,w . ,x))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-29"
  (run* (q)
    (fresh (w x)
      (numbero w)
      (=/= `(a . ,x) `(,w . b))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (num _.0))))

(test "numbero-30"
  (run* (q)
    (fresh (w x)
      (numbero w)
      (=/= `(5 . ,x) `(,w . b))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (=/= ((_.0 5) (_.1 b))) (num _.0))))

(test "numbero-31"
 (run* (q)
  (fresh (x y z a b)
    (numbero x)
    (numbero y)
    (numbero z)
    (numbero a)
    (numbero b)
    (== `(,y ,z ,x ,b) `(,z ,x ,y ,a))
    (== q `(,x ,y ,z ,a ,b))))
'(((_.0 _.0 _.0 _.1 _.1) (num _.0 _.1))))

(test "numbero-32"
 (run* (q)
  (fresh (x y z a b)
    (== q `(,x ,y ,z ,a ,b))
    (== `(,y ,z ,x ,b) `(,z ,x ,y ,a))
    (numbero x)
    (numbero a)))
 '(((_.0 _.0 _.0 _.1 _.1) (num _.0 _.1))))
