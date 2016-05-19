(test "absento 'closure-1a"
  (run* (q) (absento 'closure q) (== q 'closure))
  '())

(test "absento 'closure-1b"
  (run* (q) (== q 'closure) (absento 'closure q))
  '())

(test "absento 'closure-2a"
  (run* (q) (fresh (a d) (== q 'closure) (absento 'closure q)))
  '())

(test "absento 'closure-2b"
  (run* (q) (fresh (a d) (absento 'closure q) (== q 'closure)))
  '())

(test "absento 'closure-3a"
  (run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q)))
  '(((_.0 . _.1) (absento (closure _.0) (closure _.1)))))

(test "absento 'closure-3b"
  (run* (q) (fresh (a d) (== `(,a . ,d) q) (absento 'closure q)))  
  '(((_.0 . _.1) (absento (closure _.0) (closure _.1)))))

(test "absento 'closure-4a"
  (run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q) (== 'closure a)))
  '())

(test "absento 'closure-4b"
  (run* (q) (fresh (a d) (absento 'closure q) (== 'closure a) (== `(,a . ,d) q)))
  '())

(test "absento 'closure-4c"
  (run* (q) (fresh (a d) (== 'closure a) (absento 'closure q) (== `(,a . ,d) q)))
  '())

(test "absento 'closure-4d"
  (run* (q) (fresh (a d) (== 'closure a) (== `(,a . ,d) q) (absento 'closure q)))
  '())

(test "absento 'closure-5a"
  (run* (q) (fresh (a d) (absento 'closure q) (== `(,a . ,d) q) (== 'closure d)))
  '())

(test "absento 'closure-5b"
  (run* (q) (fresh (a d) (absento 'closure q) (== 'closure d) (== `(,a . ,d) q)))
  '())

(test "absento 'closure-5c"
  (run* (q) (fresh (a d) (== 'closure d) (absento 'closure q) (== `(,a . ,d) q)))
  '())

(test "absento 'closure-5d"
  (run* (q) (fresh (a d) (== 'closure d) (== `(,a . ,d) q) (absento 'closure q)))
  '())

(test "absento 'closure-6"
  (run* (q)
    (== `(3 (closure x (x x) ((y . 7))) #t) q)
    (absento 'closure q))
  '())
