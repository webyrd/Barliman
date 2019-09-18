(load "mkf.scm")
(load "test-check.scm")
(load "radiw.scm")

;; WEB -- need to try bit vector representation of sets, to see if we
;; can get better divergence behavior.

;;; WEB
;;; set-equivo no longer generates non-sets, but can diverge on a run 2
(test "set-equivo-0"
  (run 1 (q)
    (set-equivo '(a) q))
  '((a)))

;;; WEB run 3 diverges
(test "set-equivo-1"
  (run 2 (q)
    (set-equivo '(a b) q))
  '((a b) (b a)))

(test "set-equivo-2"
  (run 5 (q)
    (set-equivo q q))
  '(()
    (_.0)
    ((_.0 _.1)
     (=/= ((_.0 _.1))))
    ((_.0 _.1 _.2)
     (=/= ((_.0 _.1))
          ((_.0 _.2))
          ((_.1 _.2))))
    ((_.0 _.1 _.2 _.3)
     (=/= ((_.0 _.1))
          ((_.0 _.2))
          ((_.0 _.3))
          ((_.1 _.2))
          ((_.1 _.3))
          ((_.2 _.3))))))


(test "muo-0"
  (run 2 (q)
    (muo '() '() q))
  '(()))

(test "add-uo-1"
  (run 2 (q)
    (add-uo 'a '(aval (pos) ()) '() q))
  '(((a (aval (pos) ())))))

;;; WEB pos comes before neg...
(test "add-uo-2"
  (run 2 (q)
    (add-uo 'a '(aval (pos) ()) '((a (aval (neg) ()))) q))
  '(((a (aval (pos neg) ())))))
  
(test "muo-1"
  (run 2 (q)
    (muo '((a (aval (pos) ())) (b (aval (neg) ())))
         '((a (aval (neg) ()))) q))
  '(((a (aval (neg pos) ())) (b (aval (neg) ())))))

(define fact
  `(lam self n
        (if0 (var n)
             (int 1)
             (times (var n)
                    (app (var self)
                         (plus (var n) (int -1)))))))

(define efact
  `(app ,fact (int 1)))


(define fib
  `(lam self n
        (if0 (var n)
             (int 0)
             (if0 (plus (var n) (int -1))
                  (int 1)
                  (plus (app (var self)
                             (plus (var n) (int -1)))
                        (app (var self)
                             ;; WEB really should use -2 instead of -1,
                             ;; but the current abstraction doesn't
                             ;; support this!
                             (plus (var n) (int -1))))))))

(define efib
  `(app ,fib (int 1)))


(time
  (test "radiw-efib-1"
    (run* [q]
      (analyzeo efib q))
    '((aval (zer pos) ()))))

(time
  (test "radiw-efib-2"
    (run* [ie val]
      (analyzeo `(app ,fib (int ,ie)) val))
    '((0 (aval (zer) ()))
      (-1 (aval () ()))
      (1 (aval (zer pos) ())))))

(time
  (test "radiw-efib-3"
    (run* [q]
      (analyzeo `(app ,fib (int -1))
                '(aval () ()))
      (analyzeo `(app ,fib (int 0))
                '(aval (zer) ()))
      (analyzeo `(app ,fib (int 1))
                '(aval (zer pos) ())))
    '(_.0)))

(time
 (test "radiw-efib-5"
   (run 10 [e out]
     (analyzeo `(app ,fib ,e) out))
   '(((var n) (aval () ()))
     (((var _.0) (aval () ()))
      (=/= ((_.0 n)) ((_.0 self)))
      (sym _.0))
     ((var self) (aval () ()))
     ((int 0) (aval (zer) ()))
     ((lam _.0 _.1 _.2) (aval () ()))
     ((plus (var self) (var self)) (aval () ()))
     ((int -1) (aval () ()))
     ((plus (var n) (var n)) (aval () ()))
     (((plus (var _.0) (var _.0)) (aval () ()))
      (=/= ((_.0 n)) ((_.0 self)))
      (sym _.0))
     ((plus (var self) (var n)) (aval () ())))))

(time
  (test "radiw-efib-synthesis-0"
    (run* [fib]
      (== `(lam self n
                (if0 (var n)
                     (int 0)
                     (if0 (plus (var n) (int -1))
                          (int 1)
                          (plus (app (var self)
                                     (plus (var n) (int -1)))
                                (app (var self)
                                     (plus (var n) (int -1)))))))
          fib)
      (analyzeo `(app ,fib (int -1))
                '(aval () ()))
      (analyzeo `(app ,fib (int 0))
                '(aval (zer) ()))
      (analyzeo `(app ,fib (int 1))
                '(aval (zer pos) ())))
    '((lam self n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus (app (var self)
                                (plus (var n) (int -1)))
                           (app (var self)
                                (plus (var n) (int -1))))))))))

(time
  (test "radiw-efib-synthesis-1"
    (run 1 [fib]
      (fresh (r)
        (== `(lam self n
                  (if0 (var n)
                       (int 0)
                       (if0 (plus (var n) (int -1))
                            ,r
                            (plus (app (var self)
                                       (plus (var n) (int -1)))
                                  (app (var self)
                                       (plus (var n) (int -1)))))))
            fib))
      (analyzeo `(app ,fib (int -1))
                '(aval () ()))
      (analyzeo `(app ,fib (int 0))
                '(aval (zer) ()))
      (analyzeo `(app ,fib (int 1))
                '(aval (zer pos) ())))
    '((lam self n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus (app (var self)
                                (plus (var n) (int -1)))
                           (app (var self)
                                (plus (var n) (int -1))))))))))

(time
  (test "radiw-efib-synthesis-2"
    (run 1 [fib]
      (fresh (r)
        (== `(lam self n
                  (if0 (var n)
                       (int 0)
                       (if0 (plus (var n) (int -1))
                            (int 1)
                            (plus (app (var self)
                                       (plus (var n) ,r))
                                  (app (var self)
                                       (plus (var n) (int -1)))))))
            fib))
      (analyzeo `(app ,fib (int -1))
                '(aval () ()))
      (analyzeo `(app ,fib (int 0))
                '(aval (zer) ()))
      (analyzeo `(app ,fib (int 1))
                '(aval (zer pos) ())))
    '((lam self n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus (app (var self)
                                (plus (var n) (int -1)))
                           (app (var self)
                                (plus (var n) (int -1))))))))))

(time
  (test "radiw-efib-synthesis-3"
    (run 3 [fib]
      (fresh (r)
        (== `(lam self n
                  (if0 (var n)
                       (int 0)
                       (if0 (plus (var n) (int -1))
                            (int 1)
                            (plus (app (var self)
                                       (plus ,r (int -1)))
                                  (app (var self)
                                       (plus (var n) (int -1)))))))
            fib))
      (analyzeo `(app ,fib (int -1))
                '(aval () ()))
      (analyzeo `(app ,fib (int 0))
                '(aval (zer) ()))
      (analyzeo `(app ,fib (int 1))
                '(aval (zer pos) ())))
    '((lam self n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus (app (var self) (plus (int -1) (int -1)))
                           (app (var self) (plus (var n) (int -1)))))))
      (lam self n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus (app (var self) (plus (int 0) (int -1)))
                           (app (var self) (plus (var n) (int -1)))))))
      (lam self n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus (app (var self) (plus (var n) (int -1)))
                           (app (var self) (plus (var n) (int -1))))))))))

(time
  (test "radiw-efib-synthesis-4"
    (run 3 [fib]
      (fresh (r)
        (== `(lam self n
                  (if0 (var n)
                       (int 0)
                       (if0 (plus (var n) (int -1))
                            (int 1)
                            (plus (app (var self)
                                       (plus (var n) (int -1)))
                                  (app (var self)
                                       (plus ,r (int -1)))))))
            fib))
      (analyzeo `(app ,fib (int -1))
                '(aval () ()))
      (analyzeo `(app ,fib (int 0))
                '(aval (zer) ()))
      (analyzeo `(app ,fib (int 1))
                '(aval (zer pos) ())))
    '((lam self
           n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus
                      (app (var self) (plus (var n) (int -1)))
                      (app (var self) (plus (int -1) (int -1)))))))
      (lam self
           n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus
                      (app (var self) (plus (var n) (int -1)))
                      (app (var self) (plus (int 0) (int -1)))))))
      (lam self
           n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus
                      (app (var self) (plus (var n) (int -1)))
                      (app (var self) (plus (var n) (int -1))))))))))

(time
 (test "radiw-efib-synthesis-5"
   (run 2 [fib]
     (fresh (r)
       (== `(lam self n
                 (if0 (var n)
                      (int 0)
                      (if0 (plus (var n) (int -1))
                           (int 1)
                           (plus (app (var self)
                                      (plus (var n) (int -1)))
                                 (app (var self)
                                      (,r (var n) (int -1)))))))
           fib))
     (analyzeo `(app ,fib (int -1))
               '(aval () ()))
     (analyzeo `(app ,fib (int 0))
               '(aval (zer) ()))
     (analyzeo `(app ,fib (int 1))
               '(aval (zer pos) ())))
   '((lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (var n) (int -1)))
                     (app (var self) (app (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (var n) (int -1)))
                     (app (var self) (plus (var n) (int -1))))))))))

(time
 (test "radiw-efib-synthesis-6"
   (run 20 [fib]
     (fresh (r)
       (== `(lam self n
                 (if0 (var n)
                      (int 0)
                      (if0 (plus (var n) (int -1))
                           (int 1)
                           (plus (app (var self)
                                      ,r)
                                 (app (var self)
                                      (plus (var n) (int -1)))))))
           fib))
     (analyzeo `(app ,fib (int -1))
               '(aval () ()))
     (analyzeo `(app ,fib (int 0))
               '(aval (zer) ()))
     (analyzeo `(app ,fib (int 1))
               '(aval (zer pos) ())))
   '((lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (int -1))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (var n))
                     (app (var self) (plus (var n) (int -1)))))))
     ((lam self
           n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus
                      (app (var self) (var _.0))
                      (app (var self) (plus (var n) (int -1)))))))
      (=/= ((_.0 n)) ((_.0 self)))
      (sym _.0))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (var self))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (lam _.0 _.1 _.2))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (int -1) (int -1)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int -1) (int -1)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (int -1) (int 0)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int -1) (int 0)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int -1) (var n)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (int -1) (var n)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int -1) (int 1)))
                     (app (var self) (plus (var n) (int -1)))))))
     ((lam self
           n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus
                      (app (var self) (app (int -1) (var _.0)))
                      (app (var self) (plus (var n) (int -1)))))))
      (=/= ((_.0 n)) ((_.0 self)))
      (sym _.0))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int -1) (var self)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int 0) (int -1)))
                     (app (var self) (plus (var n) (int -1)))))))
     ((lam self
           n
           (if0 (var n)
                (int 0)
                (if0 (plus (var n) (int -1))
                     (int 1)
                     (plus
                      (app (var self) (plus (int -1) (var _.0)))
                      (app (var self) (plus (var n) (int -1)))))))
      (=/= ((_.0 n)) ((_.0 self)))
      (sym _.0))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int -1) (lam _.0 _.1 _.2)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (int -1) (var self)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (app (int 0) (int 0)))
                     (app (var self) (plus (var n) (int -1)))))))
     (lam self
          n
          (if0 (var n)
               (int 0)
               (if0 (plus (var n) (int -1))
                    (int 1)
                    (plus
                     (app (var self) (plus (int 0) (int -1)))
                     (app (var self) (plus (var n) (int -1))))))))))


(test "radiw-step-1"
  (run 2 [q]
    (fresh [v s c]
      (== q `(,v ,s ,c))
      (adivalpo '(int 1) '() '() '() v s c)))
  '(((aval (pos) ()) () (((int 1) (aval (pos) ()))))))

(test "radiw-step-2"
  (run 2 [q]
    (fresh [v s c]
      (== q `(,v ,s ,c))
      (adivalpo '(int 1) '() '(((int 1) (aval (pos) ()))) '() v s c)))
  '(((aval (pos) ()) () (((int 1) (aval (pos) ()))))))

(test "radiw-lffpo-1"
  (run 2 [q]
    (fresh [s c]
      (== q `(,s ,c))
      (lfppo '(int 1) '() '() s c)))
  '((() (((int 1) (aval (pos) ()))))))

(test "radiw-lffpo-2"
  (run 2 [q]
    (fresh [s c]
      (== q `(,s ,c))
      (lfppo '(int 1) '() '(((int 1) (aval (pos) ()))) s c)))
  '((() (((int 1) (aval (pos) ()))))))

(test "radiw-num-1"
  (run 2 [q]
    (analyzeo '(int 1) q))
  '((aval (pos) ())))

(test "radiw-num-2"
  (run 2 [q]
    (analyzeo '(int 0) q))
  '((aval (zer) ())))

(test "radiw-num-3"
  (run 2 [q]
    (analyzeo '(int -1) q))
  '((aval (neg) ())))

(test "radiw-plus-0"
  (run* [q]
    (analyzeo '(plus (int 1) (int 1)) q))
  '((aval (pos) ())))

(test "radiw-plus-1"
  (run* [q]
    (analyzeo '(plus (int -1) (int -1)) q))
  '((aval (neg) ())))

(test "radiw-plus-2"
  (run* [q]
    (analyzeo '(plus (int 0) (int 0)) q))
  '((aval (zer) ())))

(test "radiw-plus-3"
  (run* [q]
    (analyzeo '(plus (int 1) (int 0)) q))
  '((aval (pos) ())))

(test "radiw-plus-4"
  (run* [q]
    (analyzeo '(plus (int 0) (int -1)) q))
  '((aval (neg) ())))

(test "radiw-plus-5"
  (run* [q]
    (analyzeo '(plus (int -1) (int 1)) q))
  '((aval (neg zer pos) ())))

(test "radiw-times-0"
  (run* [q]
    (analyzeo '(times (int -1) (int 1)) q))
  '((aval (neg) ())))

(test "radiw-times-1"
  (run* [q]
    (analyzeo '(times (int -1) (int 0)) q))
  '((aval (zer) ())))

(test "radiw-times-2"
  (run* [q]
    (analyzeo '(times (int -1) (int -1)) q))
  '((aval (pos) ())))

(test "radiw-if0-0"
  (run* [q]
    (analyzeo '(if0 (int 0) (int 1) (int -1)) q))
  '((aval (pos) ())))

(test "radiw-if0-1"
  (run* [q]
    (analyzeo '(if0 (int 1) (int 1) (int -1)) q))
  '((aval (neg) ())))

(test "radiw-if0-2"
  (run* [q]
    (analyzeo '(if0 (times (int 1) (int 0)) (int 1) (int -1)) q))
  '((aval (pos) ())))

(test "radiw-if0-3"
  (run* [q]
    (analyzeo '(if0 (times (int 1) (int 1)) (int 1) (int -1)) q))
  '((aval (neg) ())))

(test "radiw-lam-0"
  (run* [q]
    (analyzeo '(lam self n (int -1)) q))
  '((aval () ((self n (int -1))))))

(test "radiw-app-0"
  (run* [q]
    (analyzeo '(app (lam self n (int -1)) (int 1)) q))
  '((aval (neg) ())))

(test "radiw-app-1"
  (run* [q]
    (analyzeo '(app (lam self n (var n)) (int 1)) q))
  '((aval (pos) ())))

(test "radiw-efact-0"
  (run 2 [q]
    (analyzeo `(app ,fact (int 0)) q))
  '((aval (pos) ())))

(test "radiw-efact"
  (run 2 [q]
    (analyzeo efact q))
  '((aval (neg zer pos) ())))
