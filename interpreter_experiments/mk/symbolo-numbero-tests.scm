(test "symbolo-numbero-1"
  (run* (q) (symbolo q) (numbero q))
  '())

(test "symbolo-numbero-2"
  (run* (q) (numbero q) (symbolo q))
  '())

(test "symbolo-numbero-3"
  (run* (q)
    (fresh (x)
      (numbero x)
      (symbolo x)))
  '())

(test "symbolo-numbero-4"
  (run* (q)
    (fresh (x)
      (symbolo x)
      (numbero x)))
  '())

(test "symbolo-numbero-5"
  (run* (q)
    (numbero q)
    (fresh (x)
      (symbolo x)
      (== x q)))
  '())

(test "symbolo-numbero-6"
  (run* (q)
    (symbolo q)
    (fresh (x)
      (numbero x)
      (== x q)))
  '())

(test "symbolo-numbero-7"
  (run* (q)
    (fresh (x)
      (numbero x)
      (== x q))
    (symbolo q))
  '())

(test "symbolo-numbero-7"
  (run* (q)
    (fresh (x)
      (symbolo x)
      (== x q))
    (numbero q))
  '())

(test "symbolo-numbero-8"
  (run* (q)
    (fresh (x)
      (== x q)
      (symbolo x))
    (numbero q))
  '())

(test "symbolo-numbero-9"
  (run* (q)
    (fresh (x)
      (== x q)
      (numbero x))
    (symbolo q))
  '())

(test "symbolo-numbero-10"
  (run* (q)
    (symbolo q)
    (fresh (x)
      (numbero x)))
  '((_.0 (sym _.0))))

(test "symbolo-numbero-11"
  (run* (q)
    (numbero q)
    (fresh (x)
      (symbolo x)))
  '((_.0 (num _.0))))

(test "symbolo-numbero-12"
  (run* (q)    
    (fresh (x y)
      (symbolo x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-numbero-13"
  (run* (q)    
    (fresh (x y)
      (numbero x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (num _.0))))

(test "symbolo-numbero-14"
  (run* (q)    
    (fresh (x y)
      (numbero x)
      (symbolo y)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (num _.0) (sym _.1))))

(test "symbolo-numbero-15"
  (run* (q)    
    (fresh (x y)
      (numbero x)
      (== `(,x ,y) q)
      (symbolo y)))
  '(((_.0 _.1) (num _.0) (sym _.1))))

(test "symbolo-numbero-16"
  (run* (q)    
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (symbolo y)))
  '(((_.0 _.1) (num _.0) (sym _.1))))

(test "symbolo-numbero-17"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (symbolo y))
    (fresh (w z)
      (== `(,w ,z) q)))
  '(((_.0 _.1) (num _.0) (sym _.1))))

(test "symbolo-numbero-18"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (symbolo y))
    (fresh (w z)
      (== `(,w ,z) q)
      (== w 5)))
  '(((5 _.0) (sym _.0))))

(test "symbolo-numbero-19"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (symbolo y))
    (fresh (w z)
      (== 'a z)
      (== `(,w ,z) q)))
  '(((_.0 a) (num _.0))))

(test "symbolo-numbero-20"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero x)
      (symbolo y))
    (fresh (w z)
      (== `(,w ,z) q)
      (== 'a z)))
  '(((_.0 a) (num _.0))))

(test "symbolo-numbero-21"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (=/= ((_.0 5) (_.1 a))))))

(test "symbolo-numbero-22"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)
      (symbolo x)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-numbero-23"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (symbolo x)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-numbero-24"
  (run* (q)
    (fresh (x y)
      (symbolo x)
      (== `(,x ,y) q)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-numbero-25"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (symbolo x)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-numbero-26"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (== `(,x ,y) q)
      (symbolo x)))
  '(((_.0 _.1) (sym _.0))))

(test "symbolo-numbero-27"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)
      (numbero y)))
  '(((_.0 _.1) (num _.1))))

(test "symbolo-numbero-28"
  (run* (q)
    (fresh (x y)
      (== `(,x ,y) q)
      (numbero y)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (num _.1))))

(test "symbolo-numbero-29"
  (run* (q)
    (fresh (x y)
      (numbero y)
      (== `(,x ,y) q)
      (=/= `(5 a) q)))
  '(((_.0 _.1) (num _.1))))

(test "symbolo-numbero-30"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (numbero y)
      (== `(,x ,y) q)))
  '(((_.0 _.1) (num _.1))))

(test "symbolo-numbero-31"
  (run* (q)
    (fresh (x y)
      (=/= `(5 a) q)
      (== `(,x ,y) q)
      (numbero y)))
  '(((_.0 _.1) (num _.1))))

(test "symbolo-numbero-32"
  (run* (q)
    (fresh (x y)
      (=/= `(,x ,y) q)
      (numbero x)
      (symbolo y)))
  '(_.0))

(test "symbolo-numbero-33"
  (run* (q)
    (fresh (x y)
      (numbero x)
      (=/= `(,x ,y) q)
      (symbolo y)))
  '(_.0))

(test "symbolo-numbero-34"
  (run* (q)
    (fresh (x y)
      (numbero x)
      (symbolo y)
      (=/= `(,x ,y) q)))
  '(_.0))
