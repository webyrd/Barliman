;;; streaming run and run* interface
;;;
;;; prints the answers as they are generated, along with the current answer count, and total elapsed wall time
;;;
;;; also returns the final answer list, as usual

(define-syntax streaming-run
  (syntax-rules ()
    ((_ n (x) g0 g ...)
     (streaming-take n 0 (time-second (current-time))
       (lambdaf@ ()
         ((fresh (x) g0 g ... purge-M-inc-models
            (lambdag@ (final-c)
              (let ((z ((reify x) final-c)))
                (choice z empty-f))))
          empty-c))))))

(define-syntax streaming-run*
  (syntax-rules ()
    ((_ (x) g ...) (streaming-run #f (x) g ...))))

(define streaming-take
  (lambda (n answer-count start-time f)
    (cond
      ((and n (zero? n)) '())
      (else
       (case-inf (f)
         (() '())
         ((f) (streaming-take n answer-count start-time f))
         ((c)
          (let ((total-elapsed-time (- (time-second (current-time)) start-time))
                (answer-count (add1 answer-count)))
            (printf "~s [answer ~s, ~s seconds total elapsed (wall time)]\n" c answer-count total-elapsed-time)
            (cons c '())))
         ((c f)
          (let ((total-elapsed-time (- (time-second (current-time)) start-time))
                (answer-count (add1 answer-count)))
            (printf "~s [answer ~s, ~s seconds total elapsed (wall time)]\n" c answer-count total-elapsed-time)
            (cons c (streaming-take (and n (- n 1)) answer-count start-time f)))))))))
