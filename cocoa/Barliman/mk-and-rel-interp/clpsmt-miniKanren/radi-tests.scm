;;(load "mk.scm")
;;(load "z3-driver.scm")
;;(load "test-check.scm")
(load "../clpsmt-miniKanren/radi.scm")

(test "set-unionso-0"
  (run* [q]
    (fresh (xs out)
      (== '((a) (b d) () (a b c)) xs)
      (== (list xs out) q)
      (set-unionso xs out)))
  '((((a) (b d) () (a b c)) (d a b c))))

(test "set-unionso2-0"
  (run* [q]
    (fresh (xs out)
      (== '(a b d a b c) xs)
      (== (list xs out) q)
      (set-unionso2 xs out)))
  '(((a b d a b c) (d a b c))))

(test "set-unionso2-1"
  (run 10 [q]
    (fresh (xs out)
      (== (list xs out) q)
      (set-unionso2 xs out)))
  '((() ())
    ((_.0) (_.0))
    ((_.0 _.0) (_.0))
    (((_.0 _.1) (_.0 _.1)) (=/= ((_.1 _.0))))
    ((_.0 _.0 _.0) (_.0))
    (((_.0 _.0 _.1) (_.0 _.1)) (=/= ((_.1 _.0))))
    (((_.0 _.1 _.0) (_.1 _.0)) (=/= ((_.1 _.0))))
    ((_.0 _.0 _.0 _.0) (_.0))
    (((_.0 _.1 _.1) (_.0 _.1)) (=/= ((_.1 _.0))))
    (((_.0 _.0 _.0 _.1) (_.0 _.1)) (=/= ((_.1 _.0))))))

(test "radi-int-0"
  (run 2 [q]
    (analyzeo `(int 0) q))
  '((((aval (zer) ())
      ()))))

(test "radi-int-1"
  (run 2 [q]
    (analyzeo `(int 1) q))
  '((((aval (pos) ())
      ()))))

(test "radi-int-2"
  (run 2 [q]
    (analyzeo `(int -1) q))
  '((((aval (neg) ())
      ()))))

(test "radi-plus-1"
  (run 2 [q]
    (analyzeo `(plus (int 1) (int 2)) q))
  '((((aval (pos) ())
      ()))))

(test "radi-plus-2"
  (run 2 [q]
    (analyzeo `(plus (int 0) (int 0)) q))
  '((((aval (zer) ())
      ()))))

(test "radi-plus-3"
  (run 2 [q]
    (analyzeo `(plus (int 0) (int 5)) q))
  '((((aval (pos) ())
      ()))))

(test "radi-plus-4"
  (run 2 [q]
    (analyzeo `(plus (int -3) (int 5)) q))
  '((((aval (neg zer pos) ())
      ()))))

(test "radi-plus-5"
  (run 2 [q]
    (analyzeo `(plus (int -3) (int 0)) q))
  '((((aval (neg) ())
      ()))))

(test "radi-plus-6"
  (run 2 [q]
    (analyzeo `(plus (int -3) (int -2)) q))
  '((((aval (neg) ())
      ()))))

(test "radi-app-plus-1"
  (run 2 [q]
    (analyzeo `(app (lam self n
                         (plus (var n) (int -1)))
                    (int 1))
              q))
  '((((aval (neg zer pos) ())
      ((self (aval () ((self n
                             (plus (var n) (int -1))))))
       (n (aval (pos) ())))))))

(test "radi-app-if-plus-1"
  (run 2 [q]
    (analyzeo `(app (lam self n
                         (if0 (var n)
                              (int 1)
                              (plus (var n) (int -1))))
                    (int 1))
              q))
  '((((aval (neg zer pos) ())
      ((self (aval () ((self n
                             (if0 (var n)
                                  (int 1)
                                  (plus (var n) (int -1)))))))
       (n (aval (pos) ())))))))

(test "radi-times-1"
  (run 2 [q]
    (analyzeo `(times (int 3) (int 2)) q))
  '((((aval (pos) ())
      ()))))

(test "radi-times-2"
  (run 2 [q]
    (analyzeo `(times (int 0) (int 2)) q))
  '((((aval (zer) ())
      ()))))

(test "radi-times-3"
  (run 2 [q]
    (analyzeo `(times (int 2) (int 0)) q))
  '((((aval (zer) ())
      ()))))

(test "radi-times-4"
  (run 2 [q]
    (analyzeo `(times (int -2) (int 0)) q))
  '((((aval (zer) ())
      ()))))

(test "radi-times-5"
  (run 2 [q]
    (analyzeo `(times (int 0) (int -2)) q))
  '((((aval (zer) ())
      ()))))

(test "radi-times-6"
  (run 2 [q]
    (analyzeo `(times (int 3) (int -2)) q))
  '((((aval (neg) ())
      ()))))

(test "radi-times-7"
  (run 2 [q]
    (analyzeo `(times (int -2) (int 3)) q))
  '((((aval (neg) ())
      ()))))

(test "radi-times-8"
  (run 2 [q]
    (analyzeo `(times (int -2) (int -3)) q))
  '((((aval (pos) ())
      ()))))

(test "radi-times-9"
  (run 2 [q]
    (analyzeo `(times (int 0) (int 0)) q))
  '((((aval (zer) ())
      ()))))

(test "radi-if0-1"
  (run 2 [q]
    (analyzeo `(if0 (int 0) (int 1) (int -1)) q))
  '((((aval (pos) ())
      ()))))

(test "radi-if0-2"
  (run 2 [q]
    (analyzeo `(if0 (int -2) (int 1) (int -1)) q))
  '((((aval (neg) ())
      ()))))

(test "radi-if0-3"
  (run 2 [q]
    (analyzeo `(if0 (times (int -2) (int 0)) (int 1) (int -1)) q))
  '((((aval (pos) ())
      ()))))

(test "radi-if0-4"
  (run 2 [q]
    (analyzeo `(if0 (times (int -2) (int 3)) (int 1) (int -1)) q))
  '((((aval (neg) ())
      ()))))

(test "radi-lam-1"
  (run 2 [q]
    (analyzeo `(lam self n (var n)) q))
  '((((aval () ((self n
                      (var n))))
      ()))))

(test "radi-app-0"
  (run 2 [q]
    (analyzeo `(plus (app (lam self y
                               (var y))
                          (int 1))
                     (int -1))
              q))
  '((((aval (neg zer pos) ())
    ((self (aval () ((self y (var y)))))
     (y (aval (pos) ())))))))

(test "radi-app-0b"
  (run 2 [q]
    (analyzeo `(plus (app (lam self y
                               (if0 (var y)
                                    (int 1)
                                    (int 1)))
                          (int 1))
                     (int -1))
              q))
  '((((aval (neg zer pos) ())
      ((self (aval () ((self y (if0 (var y) (int 1) (int 1))))))
       (y (aval (pos) ())))))))

(test "radi-app-0c"
  (run 2 [q]
    (analyzeo `(plus (app (lam self y
                               (if0 (var y)
                                    (var y)
                                    (var y)))
                          (int 1))
                     (int -1))
              q))
  '((((aval (neg zer pos) ())
      ((self (aval () ((self y (if0 (var y) (var y) (var y))))))
       (y (aval (pos) ())))))))

(test "radi-app-0d"
  (run 2 [q]
    (analyzeo `(plus (app (lam self y
                               (if0 (var y)
                                    (var y)
                                    (app (var self) (var y))))
                          (int 1))
                     (int -1))
              q))
  '(()))

(test "radi-app-0e"
  (run 2 [q]
    (analyzeo `(plus (app (lam self y
                               (if0 (var y)
                                    (var y)
                                    (plus (int 1)
                                          (app (var self) (var y)))))
                          (int 1))
                     (int -1))
              q))
  '(()))

(test "radi-app-0f"
  (run 2 [q]
    (analyzeo `(app (lam self y
                         (if0 (var y)
                              (var y)
                              (plus (int 1)
                                    (app (var self) (var y)))))
                    (int 1))
              q))
  '(()))

(test "radi-app-0g"
  (run 2 [q]
    (analyzeo `(app (lam self y
                         (if0 (var y)
                              (var y)
                              (plus (int 1)
                                    (app (var self) (int 1)))))
                    (int 1))
              q))
  '(()))

(test "radi-app-0h"
  (run 2 [q]
    (analyzeo `(app (lam self y
                         (if0 (var y)
                              (int 1)
                              (plus (int 1)
                                    (app (var self) (int 1)))))
                    (int 1))
              q))
  '(()))

(test "radi-app-0i"
  (run 2 [q]
    (analyzeo `(app (lam self y
                         (if0 (int 1)
                              (int 1)
                              (plus (int 1)
                                    (app (var self) (int 1)))))
                    (int 1))
              q))
  '(()))

(test "radi-app-0j"
  (run 2 [q]
    (analyzeo `(app (lam self y
                         (plus (int 1)
                               (app (var self) (int 1))))
                    (int 1))
              q))
  '(()))

;;; Compare this test with the previous one!
(test "radi-app-0k"
  (run 2 [q]
    (analyzeo `(app (lam self y
                         (app (var self) (int 1)))
                    (int 1))
              q))
  '(()))

(test "radi-app-1"
  (run 2 [q]
    (analyzeo `(app (lam x y (var y)) (int 1)) q))
  '((((aval (pos) ())
      ((x (aval () ((x y
                       (var y)))))
       (y (aval (pos) ())))))))

(test "radi-app-2"
  (run 2 [q]
    (analyzeo `(app (lam self n (var n)) (int 1)) q))
  '((((aval (pos) ())
      ((self (aval () ((self n
                             (var n)))))
       (n (aval (pos) ())))))))

(test "radi-app-3"
  (run 2 [q]
    (analyzeo `(app (lam self n (times (var n) (var n))) (int -3)) q))
  '((((aval (pos) ())
      ((self (aval () ((self n
                             (times (var n) (var n))))))
       (n (aval (neg) ())))))))

(test "radi-loop-1"
  (run 2 [q]
    (analyzeo `(app (lam self n (app (var self) (var n))) (int 1)) q))
  '(()))

(test "radi-loop-1a"
  (run 2 [q]
    (analyzeo `(app (lam self n (if0 (var n) (app (var self) (var n)) (app (var self) (var n)))) (int 1)) q))
  '(()))

(test "radi-loop-2a"
  (run 2 [q]
    (analyzeo `(app (lam self n (if0 (var n) (int 1) (app (var self) (var n)))) (int 1)) q))
  '(()))

(test "radi-loop-2b"
  (run 2 [q]
    (analyzeo `(app (lam self n (app (var self) (plus (var n) (int -1)))) (int 1)) q))
  '(()))

(test "radi-loop-2c"
  (run 2 [q]
    (analyzeo `(app (lam self n (if0 (var n) (int 1) (app (var self) (plus (var n) (int 1))))) (int 1)) q))
  '(()))

(test "radi-loop-2d"
  (run 2 [q]
    (analyzeo `(app (lam self n (if0 (var n) (int 1) (app (var self) (plus (var n) (int 0))))) (int 1)) q))
  '(()))

(printf "warning--this test takes a while!\n")
(time-test "radi-loop-2"
   (run 2 [q]
     (analyzeo `(app (lam self n
                          (if0 (var n)
                               (int 1)
                               (app (var self)
                                    (plus (var n) (int -1)))))
                     (int 1))
               q))
   '((((aval (pos) ())
       ((self (aval () ((self n
                              (if0 (var n)
                                   (int 1)
                                   (app (var self)
                                        (plus (var n) (int -1))))))))
        (n (aval (neg zer pos) ())))))))

(test "radi-not-a-loop-1"
  (run 2 [q]
    (analyzeo `(app (lam self n
                         (if0 (var n)
                              (int 1)
                              (plus (int 1)
                                    (plus (var n) (int -1)))))
                    (int 1))
              q))
  '((((aval (neg zer pos) ())
      ((self
        (aval
         ()
         ((self
           n
           (if0 (var n)
                (int 1)
                (plus (int 1) (plus (var n) (int -1))))))))
       (n (aval (pos) ())))))))

(printf "warning--this test takes a while!\n")
(time-test "radi-loop-3"
   (run 2 [q]
     (analyzeo `(app (lam self n
                          (if0 (var n)
                               (int 1)
                               (plus (int 1)
                                     (app (var self)
                                          (plus (var n) (int -1))))))
                     (int 1))
               q))
   '((((aval (pos) ())
       ((self
         (aval
          ()
          ((self
            n
            (if0 (var n)
                 (int 1)
                 (plus
                  (int 1)
                  (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (neg zer pos) ())))))))

(printf "warning--this test takes a while!\n")
(time-test "radi-loop-3b"
   (run 2 [q]
     (analyzeo `(app (lam self n
                          (if0 (var n)
                               (int 1)
                               (plus (int 0)
                                     (app (var self)
                                          (plus (var n) (int -1))))))
                     (int 1))
               q))
   '((((aval (pos) ())
       ((self
         (aval
          ()
          ((self
            n
            (if0 (var n)
                 (int 1)
                 (plus
                  (int 0)
                  (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (neg zer pos) ())))))))

(test "radi-loop-3c"
  (run 2 [q]
    (analyzeo `(app (lam self n
                         (if0 (var n)
                              (int 1)
                              (plus (int 1)
                                    (app (var self)
                                         (var n)))))
                    (int 1))
              q))
  '(()))

(test "radi-loop-3d"
  (run 2 [q]
    (analyzeo `(app (lam self n
                         (if0 (var n)
                              (int 1)
                              (plus (int 1)
                                    (app (var self)
                                         (int 1)))))
                    (int 1))
              q))
  '(()))

(define fact
  `(lam self n
        (if0 (var n)
             (int 1)
             (times (var n)
                    (app
                     (var self)
                     (plus (var n)
                           (int -1)))))))

(test "radi-fact-1"
  (run 2 [q]
    (analyzeo fact q))
  '((((aval () ((self n
                      (if0 (var n)
                           (int 1)
                           (times (var n)
                                  (app (var self) (plus (var n) (int -1))))))))
      ()))))

(test "radi-fact-app-0"
  (run 2 [q]
    (analyzeo `(app ,fact (int 0)) q))
  '((((aval (pos) ())
      ((self (aval () ((self n
                             (if0 (var n)
                                  (int 1)
                                  (times (var n)
                                         (app (var self) (plus (var n) (int -1)))))))))
       (n (aval (zer) ())))))))

(define efact
  `(app ,fact (int 5)))

(printf "warning--this test takes a while!\n")
(time-test "radi-efact-1"
   (run 1 [q]
     (analyzeo efact q))
   '((((aval (neg zer pos) ())
       ((self
         (aval
          ()
          ((self
            n
            (if0 (var n)
                 (int 1)
                 (times
                  (var n)
                  (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (neg zer pos) ()))))
      ((aval (pos) ())
       ((self
         (aval
          ()
          ((self
            n
            (if0 (var n)
                 (int 1)
                 (times
                  (var n)
                  (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (neg zer pos) ())))))))


(test "radi-fact-app-2"
  (run 3 [q]
    (fresh (e n out a d)
      (== (list e out) q)
      (== `(,a . ,d) out)
      (== `(app ,fact ,n) e)
      (analyzeo e out)))
  '(((app (lam self n
               (if0 (var n)
                    (int 1)
                    (times (var n)
                           (app (var self) (plus (var n) (int -1))))))
          (int 0))
     (((aval (pos) ())
       ((self (aval () ((self n
                              (if0 (var n)
                                   (int 1)
                                   (times (var n)
                                          (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (zer) ()))))))
    ((app (lam self n
               (if0 (var n)
                    (int 1)
                    (times (var n)
                           (app (var self) (plus (var n) (int -1))))))
          (plus (int 0) (int 0)))
     (((aval (pos) ())
       ((self (aval () ((self n
                              (if0 (var n)
                                   (int 1)
                                   (times (var n)
                                          (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (zer) ()))))))
    ((app (lam self n
               (if0 (var n)
                    (int 1)
                    (times (var n)
                           (app (var self) (plus (var n) (int -1))))))
          (times (int 0) (int 0)))
     (((aval (pos) ())
       ((self (aval () ((self n
                              (if0 (var n)
                                   (int 1)
                                   (times (var n)
                                          (app (var self) (plus (var n) (int -1)))))))))
        (n (aval (zer) ()))))))))

