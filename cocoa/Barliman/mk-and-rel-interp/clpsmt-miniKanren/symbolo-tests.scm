(test "symbolo-1"
  (run* (q) (symbolo q))
  '((_.0 (sym _.0))))

(test "symbolo-2"
  (run* (q) (symbolo q) (== 'x q))
  '(x))

(test "symbolo-3"
  (run* (q) (== 'x q) (symbolo q))
  '(x))

(test "symbolo-4"
  (run* (q) (== 5 q) (symbolo q))
  '())

(test "symbolo-5"
  (run* (q) (symbolo q) (== 5 q))
  '())

(test "symbolo-6"
  (run* (q) (symbolo q) (== `(1 . 2) q))
  '())

(test "symbolo-7"
  (run* (q) (== `(1 . 2) q) (symbolo q))
  '())

(test "symbolo-8"
  (run* (q) (fresh (x) (symbolo x)))
  '(_.0))

(test "symbolo-9"
  (run* (q) (fresh (x) (symbolo x)))
  '(_.0))

(test "symbolo-10"
  (run* (q) (fresh (x) (symbolo x) (== x q)))
  '((_.0 (sym _.0))))

(test "symbolo-11"
  (run* (q) (fresh (x) (symbolo q) (== x q) (symbolo x)))
  '((_.0 (sym _.0))))

(test "symbolo-12"
  (run* (q) (fresh (x) (symbolo q) (symbolo x) (== x q)))
  '((_.0 (sym _.0))))

(test "symbolo-13"
  (run* (q) (fresh (x) (== x q) (symbolo q) (symbolo x)))
  '((_.0 (sym _.0))))

(test "symbolo-14-a"
  (run* (q) (fresh (x) (symbolo q) (== 'y x)))
  '((_.0 (sym _.0))))

(test "symbolo-14-b"
  (run* (q) (fresh (x) (symbolo q) (== 'y x) (== x q)))
  '(y))

(test "symbolo-15"
  (run* (q) (fresh (x) (== q x) (symbolo q) (== 5 x)))
  '())

(test "symbolo-16-a"
  (run* (q) (symbolo q) (=/= 5 q))
  '((_.0 (sym _.0))))

(test "symbolo-16-b"
  (run* (q) (=/= 5 q) (symbolo q))
  '((_.0 (sym _.0))))

(test "symbolo-17"
  (run* (q) (symbolo q) (=/= `(1 . 2) q))
  '((_.0 (sym _.0))))

(test "symbolo-18"
  (run* (q) (symbolo q) (=/= 'y q))
  '((_.0 (=/= ((_.0 y))) (sym _.0))))

(test "symbolo-19"
  (run* (q)
    (fresh (x y)
      (symbolo x)
      (symbolo y)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (sym _.0 _.1))))

(test "symbolo-20"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (symbolo x)
      (symbolo y)))
  '(((_.0 _.1) (sym _.0 _.1))))

(test "symbolo-21"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (symbolo x)
      (symbolo x)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-22"
  (run* (q)
    (fresh (x y)
      (symbolo x)
      (symbolo x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-23"
  (run* (q)
    (fresh (x y)
      (symbolo x)
      (== `(,x ,y) q)
      (symbolo x)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-24-a"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (symbolo w)
      (symbolo z)))
  '(_.0))

(test "symbolo-24-b"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (symbolo w)
      (symbolo z)
      (== `(,w ,x ,y ,z) q)))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.0 _.2) (_.1 _.3)))
     (sym _.0 _.3))))

(test "symbolo-24-c"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (symbolo w)
      (symbolo y)
      (== `(,w ,x ,y ,z) q)))
  '(((_.0 _.1 _.2 _.3)
     (=/= ((_.0 _.2) (_.1 _.3)))
     (sym _.0 _.2))))

(test "symbolo-24-d"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w . ,x) `(,y . ,z))
      (symbolo w)
      (symbolo y)
      (== w y)
      (== `(,w ,x ,y ,z) q)))
  '(((_.0 _.1 _.0 _.2)
     (=/= ((_.1 _.2)))
     (sym _.0))))

(test "symbolo-25"
  (run* (q)
    (fresh (w x)
      (=/= `(,w . ,x) `(5 . 6))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (=/= ((_.0 5) (_.1 6))))))

(test "symbolo-26"
  (run* (q)
    (fresh (w x)
      (=/= `(,w . ,x) `(5 . 6))
      (symbolo w)
      (== `(,w ,x) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-27"
  (run* (q)
    (fresh (w x)
      (symbolo w)
      (=/= `(,w . ,x) `(5 . 6))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-28"
  (run* (q)
    (fresh (w x)
      (symbolo w)
      (=/= `(5 . 6) `(,w . ,x))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-29"
  (run* (q)
    (fresh (w x)
      (symbolo w)
      (=/= `(5 . ,x) `(,w . 6))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-30"
  (run* (q)
    (fresh (w x)
      (symbolo w)
      (=/= `(z . ,x) `(,w . 6))
      (== `(,w ,x) q)))
  '(((_.0 _.1) (=/= ((_.0 z) (_.1 6))) (sym _.0))))

(test "symbolo-31-a"
  (run* (q)
    (fresh (w x y z)
      (== x 5)
      (=/= `(,w ,y) `(,x ,z))
      (== w 5)
      (== `(,w ,x ,y ,z) q)))
  '(((5 5 _.0 _.1) (=/= ((_.0 _.1))))))

(test "symbolo-31-b"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w ,y) `(,x ,z))
      (== w 5)
      (== x 5)
      (== `(,w ,x ,y ,z) q)))
  '(((5 5 _.0 _.1) (=/= ((_.0 _.1))))))

(test "symbolo-31-c"
  (run* (q)
    (fresh (w x y z)
      (== w 5)
      (=/= `(,w ,y) `(,x ,z))
      (== `(,w ,x ,y ,z) q)
      (== x 5)))
  '(((5 5 _.0 _.1) (=/= ((_.0 _.1))))))

(test "symbolo-31-d"
  (run* (q)
    (fresh (w x y z)
      (== w 5)
      (== x 5)
      (=/= `(,w ,y) `(,x ,z))
      (== `(,w ,x ,y ,z) q)))
  '(((5 5 _.0 _.1) (=/= ((_.0 _.1))))))


(test "symbolo-32-a"
  (run* (q)
    (fresh (w x y z)
      (== x 'a)
      (=/= `(,w ,y) `(,x ,z))
      (== w 'a)
      (== `(,w ,x ,y ,z) q)))
  '(((a a _.0 _.1) (=/= ((_.0 _.1))))))

(test "symbolo-32-b"
  (run* (q)
    (fresh (w x y z)
      (=/= `(,w ,y) `(,x ,z))
      (== w 'a)
      (== x 'a)
      (== `(,w ,x ,y ,z) q)))
  '(((a a _.0 _.1) (=/= ((_.0 _.1))))))

(test "symbolo-32-c"
  (run* (q)
    (fresh (w x y z)
      (== w 'a)
      (=/= `(,w ,y) `(,x ,z))
      (== `(,w ,x ,y ,z) q)
      (== x 'a)))
  '(((a a _.0 _.1) (=/= ((_.0 _.1))))))

(test "symbolo-32-d"
  (run* (q)
    (fresh (w x y z)
      (== w 'a)
      (== x 'a)
      (=/= `(,w ,y) `(,x ,z))
      (== `(,w ,x ,y ,z) q)))
  '(((a a _.0 _.1) (=/= ((_.0 _.1))))))
