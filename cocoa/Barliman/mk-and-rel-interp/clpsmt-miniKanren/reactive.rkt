#lang racket

#|
(bouncing-ball)

(clp-bouncing-ball)

(clp-bouncing-ball-var)

(clp-bouncing-ball-var-direction 'forward)
|#

(require "mk.rkt")
(require graphics/graphics)

(provide (all-from-out "mk.rkt")
         (all-defined-out))

(do-defer-smt-checks!)
(when get-next-model? (toggle-get-next-model?!))

(define pi 3.14159)
(define two-pi (* pi 2))

;;; window size
(define horiz 300)
(define vert 300)

(define horiz-center (quotient horiz 2))
(define vert-center (quotient vert 2))


(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))

(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== (cons y rest) ls)
      (conde
        [(== x y)]
        [(=/= x y)
         (membero x rest)]))))

(define (clp-bouncing-ball-var-direction direction)
  (open-graphics)
  (let ((w (open-viewport "clp-bouncing-ball-var-direction" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0)
              (end-time 100))
          
          (define bounce-stepo
            (lambda (time-in ball-in time-out ball-out)
              (fresh (x-in y-in x-vel-in y-vel-in color-in
                      x-out y-out x-vel-out y-vel-out color-out
                      possible-new-x possible-new-y)                
                (== `(,x-in ,y-in ,x-vel-in ,y-vel-in ,color-in) ball-in)
                (== `(,x-out ,y-out ,x-vel-out ,y-vel-out ,color-out) ball-out)
                (== color-in color-out)
                (z/assert `(and (= (+ ,time-in 1) ,time-out)
                                (= (+ ,x-in ,x-vel-in) ,possible-new-x)
                                (= (+ ,y-in ,y-vel-in) ,possible-new-y)
                                (= (+ ,x-in ,x-vel-out) ,x-out)
                                (= (+ ,y-in ,y-vel-out) ,y-out)))
                (conde
                  [(== x-vel-in x-vel-out)
                   (z/assert `(and (>= ,possible-new-x ,left-wall-x)
                                   (<= ,possible-new-x ,right-wall-x)))]
                  [(=/= x-vel-in x-vel-out)
                   (z/assert `(and (not (and (>= ,possible-new-x ,left-wall-x)
                                        (<= ,possible-new-x ,right-wall-x)))
                                   (= (- ,x-vel-in) ,x-vel-out)))])
                (conde
                  [(== y-vel-in y-vel-out)
                   (z/assert `(and (>= ,possible-new-y ,top-wall-y)
                                   (<= ,possible-new-y ,bottom-wall-y)))]
                  [(=/= y-vel-in y-vel-out)
                   (z/assert `(and (not (and (>= ,possible-new-y ,top-wall-y)
                                        (<= ,possible-new-y ,bottom-wall-y)))
                                   (= (- ,y-vel-in) ,y-vel-out)))]))))

          (let loop ((start #t)
                     (new-time 'dummy)
                     (new-ball* 'dummy))

            (let ((ans (run* (q)
                         (fresh (time ball balls time^ ball^)

                           (conde
                             [(== 'forward direction)
                              (== (list time^ ball^) q)]
                             [(== 'backward direction)
                              (== (list time ball) q)])
                           
                           (conde

                             #|
                             [(== #t start)
                              (fresh (x-vel y-vel color)
                                ;; x y x-velocity y-velocity color
                                (== `((100 43 ,x-vel ,y-vel ,color))
                                    balls)

                                (z/assert `(and (>= ,x-vel -3)
                                                (<= ,x-vel 3)))

                                (conde
                                  [(z/assert `(and (= (mod (abs ,x-vel) 3) 0)
                                                   (= ,x-vel ,y-vel)))
                                   (== "red" color)]
                                  [(z/assert `(and (= (mod (abs ,x-vel) 3) 1)
                                                   (= (- ,x-vel) ,y-vel)))
                                   (== "blue" color)]
                                  [(z/assert `(and (= (mod (abs ,x-vel) 3) 2)
                                                   (= (* ,x-vel 2) ,y-vel)))
                                   (== "purple" color)])
                                
                                )]
                             |#

                             [(== #t start)
                              (fresh (y)
                                (== `((150 ,y 10 5 "blue"))
                                    balls)

                                (z/assert `(and (>= ,y 100)
                                                (<= ,y 160)
                                                (= (mod ,y 20) 0)))
                                
                                )]

                             #|
                             [(== #t start)
                              (fresh (y)
                                (== `((150 100 20 5 "blue"))
                                    balls))]
                             |#
                             
                             [(== #f start)
                              (== new-ball* balls)])

                           (conde
                             [(== #t start)
                              (conde
                                [(== 'forward direction)
                                 (== start-time time)]
                                [(== 'backward direction)
                                 (== end-time time^)])]
                             [(== #f start)
                              (conde
                                [(== 'forward direction)
                                 (== new-time time)]
                                [(== 'backward direction)
                                 (== new-time time^)])])
                           
                           (conde
                             [(== 'forward direction)
                              (membero ball balls)]
                             [(== 'backward direction)
                              (membero ball^ balls)])                           
                           
                           (bounce-stepo time ball time^ ball^)))))              
              (let ((new-time (caar ans))
                    (new-ball* (map (lambda (a)
                                      (match a
                                        [`(,t ,new-ball)
                                         new-ball]))
                                    ans)))
                
                (for-each (lambda (ball)
                            (match ball
                              [`(,x ,y ,x-vel ,y-vel ,color)
                               ((draw-solid-ellipse w)
                                (make-posn x y)
                                ball-size
                                ball-size
                                color)]))
                          new-ball*)
                
                ;;(sleep 0.05)

                (loop #f new-time new-ball*)))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (clp-bouncing-ball-var)
  (open-graphics)
  (let ((w (open-viewport "clp-bouncing-ball-var" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0))
          
          (define bounce-stepo
            (lambda (time-in ball-in time-out ball-out)
              (fresh (x-in y-in x-vel-in y-vel-in color-in
                      x-out y-out x-vel-out y-vel-out color-out
                      possible-new-x possible-new-y)                
                (== `(,x-in ,y-in ,x-vel-in ,y-vel-in ,color-in) ball-in)
                (== `(,x-out ,y-out ,x-vel-out ,y-vel-out ,color-out) ball-out)
                (== color-in color-out)
                (z/assert `(and (= (+ ,time-in 1) ,time-out)
                                (= (+ ,x-in ,x-vel-in) ,possible-new-x)
                                (= (+ ,y-in ,y-vel-in) ,possible-new-y)
                                (= (+ ,x-in ,x-vel-out) ,x-out)
                                (= (+ ,y-in ,y-vel-out) ,y-out)))
                (conde
                  [(== x-vel-in x-vel-out)
                   (z/assert `(and (>= ,possible-new-x ,left-wall-x)
                                   (<= ,possible-new-x ,right-wall-x)))]
                  [(=/= x-vel-in x-vel-out)
                   (z/assert `(and (not (and (>= ,possible-new-x ,left-wall-x)
                                        (<= ,possible-new-x ,right-wall-x)))
                                   (= (- ,x-vel-in) ,x-vel-out)))])
                (conde
                  [(== y-vel-in y-vel-out)
                   (z/assert `(and (>= ,possible-new-y ,top-wall-y)
                                   (<= ,possible-new-y ,bottom-wall-y)))]
                  [(=/= y-vel-in y-vel-out)
                   (z/assert `(and (not (and (>= ,possible-new-y ,top-wall-y)
                                        (<= ,possible-new-y ,bottom-wall-y)))
                                   (= (- ,y-vel-in) ,y-vel-out)))]))))

          (let loop ((start #t)
                     (time start-time)
                     (ball* 'dummy))

            (let ((ans (run* (q)
                         (fresh (ball balls time^ ball^)
                           (== (list time^ ball^) q)
                           (conde

                             [(== #t start)
                              (fresh (x-vel y-vel color)
                                ;; x y x-velocity y-velocity color
                                (== `((100 43 ,x-vel ,y-vel ,color))
                                    balls)

                                (z/assert `(and (>= ,x-vel -3)
                                                (<= ,x-vel 3)))

                                (conde
                                  [(z/assert `(and (= (mod (abs ,x-vel) 3) 0)
                                                   (= ,x-vel ,y-vel)))
                                   (== "red" color)]
                                  [(z/assert `(and (= (mod (abs ,x-vel) 3) 1)
                                                   (= (- ,x-vel) ,y-vel)))
                                   (== "blue" color)]
                                  [(z/assert `(and (= (mod (abs ,x-vel) 3) 2)
                                                   (= (* ,x-vel 2) ,y-vel)))
                                   (== "purple" color)])
                                
                                )]

                             #|
                             [(== #t start)
                              (fresh (y)
                                (== `((0 ,y 2 3 "blue"))
                                    balls)

                                (z/assert `(and (>= ,y 0)
                                                (<= ,y 50)
                                                (= (mod ,y 10) 0)))
                                
                                )]
                             |#
                             
                             [(== #f start)
                              (== ball* balls)])
                           (membero ball balls)
                           (bounce-stepo time ball time^ ball^)))))              
              (let ((new-time (caar ans))
                    (new-ball* (map (lambda (a)
                                      (match a
                                        [`(,t ,new-ball)
                                         new-ball]))
                                    ans)))
                
                (for-each (lambda (ball)
                            (match ball
                              [`(,x ,y ,x-vel ,y-vel ,color)
                               ((draw-solid-ellipse w)
                                (make-posn x y)
                                ball-size
                                ball-size
                                color)]))
                          new-ball*)
                    
                ;;(sleep 0.05)

                (loop #f new-time new-ball*)))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (clp-bouncing-ball)
  (open-graphics)
  (let ((w (open-viewport "clp-bouncing-ball" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0))
          
          (define bounce-stepo
            (lambda (time-in ball-in time-out ball-out)
              (fresh (x-in y-in x-vel-in y-vel-in color-in
                      x-out y-out x-vel-out y-vel-out color-out
                      possible-new-x possible-new-y)                
                (== `(,x-in ,y-in ,x-vel-in ,y-vel-in ,color-in) ball-in)
                (== `(,x-out ,y-out ,x-vel-out ,y-vel-out ,color-out) ball-out)
                (== color-in color-out)
                (z/assert `(and (= (+ ,time-in 1) ,time-out)
                                (= (+ ,x-in ,x-vel-in) ,possible-new-x)
                                (= (+ ,y-in ,y-vel-in) ,possible-new-y)
                                (= (+ ,x-in ,x-vel-out) ,x-out)
                                (= (+ ,y-in ,y-vel-out) ,y-out)))
                (conde
                  [(== x-vel-in x-vel-out)
                   (z/assert `(and (>= ,possible-new-x ,left-wall-x)
                                   (<= ,possible-new-x ,right-wall-x)))]
                  [(=/= x-vel-in x-vel-out)
                   (z/assert `(and (not (and (>= ,possible-new-x ,left-wall-x)
                                        (<= ,possible-new-x ,right-wall-x)))
                                   (= (- ,x-vel-in) ,x-vel-out)))])
                (conde
                  [(== y-vel-in y-vel-out)
                   (z/assert `(and (>= ,possible-new-y ,top-wall-y)
                                   (<= ,possible-new-y ,bottom-wall-y)))]
                  [(=/= y-vel-in y-vel-out)
                   (z/assert `(and (not (and (>= ,possible-new-y ,top-wall-y)
                                        (<= ,possible-new-y ,bottom-wall-y)))
                                   (= (- ,y-vel-in) ,y-vel-out)))]))))

          (let loop ((time start-time)
                     ;; x y x-velocity y-velocity color
                     (ball* `((0 7 2 3 "red")
                              (7 2 4 1 "green")
                              (100 100 -1 -2 "blue"))))

            (let ((ans (run* (q)
                         (fresh (ball time^ ball^)
                           (== (list time^ ball^) q)
                           (membero ball ball*)
                           (bounce-stepo time ball time^ ball^)))))              
              (let ((new-time (caar ans))
                    (new-ball* (map (lambda (a)
                                      (match a
                                        [`(,t ,new-ball)
                                         new-ball]))
                                    ans)))
                
                (for-each (lambda (ball)
                            (match ball
                              [`(,x ,y ,x-vel ,y-vel ,color)
                               ((draw-solid-ellipse w)
                                (make-posn x y)
                                ball-size
                                ball-size
                                color)]))
                          new-ball*)
                    
                ;;(sleep 0.05)

                (loop new-time new-ball*)))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (clp-bouncing-ball-slow)
  (open-graphics)
  (let ((w (open-viewport "clp-bouncing-ball-slow" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0))
          
          (define bounce-stepo
            (lambda (time-in ball-in time-out ball-out)
              (fresh (x-in y-in x-vel-in y-vel-in color-in
                      x-out y-out x-vel-out y-vel-out color-out
                      possible-new-x possible-new-y)                
                (== `(,x-in ,y-in ,x-vel-in ,y-vel-in ,color-in) ball-in)
                (== `(,x-out ,y-out ,x-vel-out ,y-vel-out ,color-out) ball-out)
                (== color-in color-out)
                (z/assert `(= (+ ,time-in 1) ,time-out))
                (z/assert `(= (+ ,x-in ,x-vel-in) ,possible-new-x))
                (z/assert `(= (+ ,y-in ,y-vel-in) ,possible-new-y))
                (z/assert `(= (+ ,x-in ,x-vel-out) ,x-out))
                (z/assert `(= (+ ,y-in ,y-vel-out) ,y-out))
                (conde
                  [(== x-vel-in x-vel-out)
                   (z/assert `(and (>= ,possible-new-x ,left-wall-x)
                                   (<= ,possible-new-x ,right-wall-x)))]
                  [(z/assert `(= (- ,x-vel-in) ,x-vel-out))
                   (z/assert `(not (and (>= ,possible-new-x ,left-wall-x)
                                        (<= ,possible-new-x ,right-wall-x))))])
                (conde
                  [(== y-vel-in y-vel-out)
                   (z/assert `(and (>= ,possible-new-y ,top-wall-y)
                                   (<= ,possible-new-y ,bottom-wall-y)))]
                  [(z/assert `(= (- ,y-vel-in) ,y-vel-out))
                   (z/assert `(not (and (>= ,possible-new-y ,top-wall-y)
                                        (<= ,possible-new-y ,bottom-wall-y))))]))))

          (let loop ((time start-time)
                     ;; x y x-velocity y-velocity color
                     (ball* `((0 7 2 3 "red"))))

            (let ((ans (run* (q)
                         (fresh (ball time^ ball^)
                           (== (list time^ ball^) q)
                           (membero ball ball*)
                           (bounce-stepo time ball time^ ball^)))))              
              (let ((new-time (caar ans))
                    (new-ball* (map (lambda (a)
                                      (match a
                                        [`(,t ,new-ball)
                                         new-ball]))
                                    ans)))
                
                (for-each (lambda (ball)
                            (match ball
                              [`(,x ,y ,x-vel ,y-vel ,color)
                               ((draw-solid-ellipse w)
                                (make-posn x y)
                                ball-size
                                ball-size
                                color)]))
                          new-ball*)
                    
                (sleep 0.05)

                (loop new-time new-ball*)))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (old-clp-bouncing-ball)
  (open-graphics)
  (let ((w (open-viewport "old-clp-bouncing-ball" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0))
          (let loop ((old-time start-time)
                     (old-ball `(0 7 2 3 "red") ; x y x-velocity y-velocity color
                               ))
            (sleep 0.05)
            (let ((new-time (add1 old-time)))

              #|
              (match old-ball
                [`(,x ,y ,x-vel ,y-vel ,color)
                 ((clear-solid-ellipse w)
                  (make-posn x y)
                  ball-size
                  ball-size)])
              |#

              (match old-ball
                [`(,x ,y ,x-vel ,y-vel ,color)
                 (let ((possible-new-x (+ x x-vel))
                       (possible-new-y (+ y y-vel)))
                   (let ((x-vel (if (and
                                     (>= possible-new-x left-wall-x)
                                     (<= possible-new-x right-wall-x))
                                    x-vel
                                    (- x-vel)))
                         (y-vel (if (and
                                     (>= possible-new-y top-wall-y)
                                     (<= possible-new-y bottom-wall-y))
                                    y-vel
                                    (- y-vel))))
                     (let ((new-x (+ x x-vel))
                           (new-y (+ y y-vel)))
                       ((draw-solid-ellipse w)
                        (make-posn new-x new-y)
                        ball-size
                        ball-size
                        color)
                       
                       (let ((new-ball (list new-x new-y x-vel y-vel color)))
                         (loop new-time new-ball)))))]))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (bouncing-ball)
  (open-graphics)
  (let ((w (open-viewport "bouncing-ball" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time 0))
          (let loop ((old-time start-time)
                     (old-ball* `((0 0 2 3) ; x y x-velocity y-velocity
                                  (7 2 4 1)
                                  (100 100 -1 -2))))
            (sleep 0.05)
            (let ((new-time (add1 old-time)))

              #|
              (for-each (lambda (ball)
                          (match ball
                            [`(,x ,y ,x-vel ,y-vel)
                             ((clear-solid-ellipse w)
                              (make-posn x y)
                              ball-size
                              ball-size)]))
                        old-ball*)
              |#
              
              (let ((new-ball* (map (lambda (ball)
                                      (match ball
                                        [`(,x ,y ,x-vel ,y-vel)
                                         (let ((possible-new-x (+ x x-vel))
                                               (possible-new-y (+ y y-vel)))
                                           (let ((x-vel (if (and
                                                             (>= possible-new-x left-wall-x)
                                                             (<= possible-new-x right-wall-x))
                                                            x-vel
                                                            (- x-vel)))
                                                 (y-vel (if (and
                                                             (>= possible-new-y top-wall-y)
                                                             (<= possible-new-y bottom-wall-y))
                                                            y-vel
                                                            (- y-vel))))
                                             (let ((new-x (+ x x-vel))
                                                   (new-y (+ y y-vel)))
                                               ((draw-solid-ellipse w)
                                                (make-posn new-x new-y)
                                                ball-size
                                                ball-size)
                                                       
                                               (let ((new-ball (list new-x new-y x-vel y-vel)))
                                                 new-ball))))]))
                                    old-ball*)))
                (loop new-time new-ball*)))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (draw-bouncing-ball-by-time)
  (open-graphics)
  (let ((w (open-viewport "draw-bouncing-ball-by-time" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)
              (left-wall-x 0)
              (top-wall-y 0)
              (right-wall-x 295)
              (bottom-wall-y 295)
              (start-time (modulo (quotient (current-milliseconds) 50) 50)))
          (let loop ((old-time start-time)
                     (old-ball* `((0 0 2 3) ; x y x-velocity y-velocity
                                  (7 2 4 1)
                                  (100 100 -1 -2))))
            (let ((new-time (modulo (quotient (current-milliseconds) 50) 50)))
              (if (= new-time old-time)
                  (loop old-time old-ball*)
                  (begin
                    #|
                    (for-each (lambda (ball)
                                (match ball
                                  [`(,x ,y ,x-vel ,y-vel)
                                   ((clear-solid-ellipse w)
                                    (make-posn x y)
                                    ball-size
                                    ball-size)]))
                              old-ball*)
                    |#
                    (let ((new-ball* (map (lambda (ball)
                                            (match ball
                                              [`(,x ,y ,x-vel ,y-vel)
                                               (let ((possible-new-x (+ x x-vel))
                                                     (possible-new-y (+ y y-vel)))
                                                 (let ((x-vel (if (and
                                                                   (>= possible-new-x left-wall-x)
                                                                   (<= possible-new-x right-wall-x))
                                                                  x-vel
                                                                  (- x-vel)))
                                                       (y-vel (if (and
                                                                   (>= possible-new-y top-wall-y)
                                                                   (<= possible-new-y bottom-wall-y))
                                                                  y-vel
                                                                  (- y-vel))))
                                                   (let ((new-x (+ x x-vel))
                                                         (new-y (+ y y-vel)))
                                                     ((draw-solid-ellipse w)
                                                      (make-posn new-x new-y)
                                                      ball-size
                                                      ball-size)
                                                       
                                                     (let ((new-ball (list new-x new-y x-vel y-vel)))
                                                       new-ball))))]))
                                          old-ball*)))
                      (loop new-time new-ball*)))))))
        (begin
          (close-viewport w)
          (close-graphics)))))

(define (old-draw-bouncing-ball-by-time)
  (open-graphics)
  (let ((w (open-viewport "old-draw-bouncing-ball-by-time" horiz vert)))
    (dynamic-wind
        void
        (let ((ball-size 5.0)              
              (start-time (modulo (quotient (current-milliseconds) 50) 50)))
          (let loop ((old-time start-time)
                     (old-pos (make-posn 0 0))
                     (x-velocity 2)
                     (y-velocity 3))
            (let ((new-time (modulo (quotient (current-milliseconds) 50) 50)))
              (if (= new-time old-time)
                  (loop old-time old-pos x-velocity y-velocity)
                  (begin

                    #|
                    ((clear-solid-ellipse w)
                     old-pos
                     ball-size
                     ball-size)
                    |#
                    
                    (let ((possible-new-x (+ (posn-x old-pos) x-velocity)))
                      (let ((x-velocity (if (<= possible-new-x 50)
                                            x-velocity
                                            (- x-velocity))))
                        (let ((new-x (+ (posn-x old-pos) x-velocity)))
                          (let ((new-pos (make-posn new-x
                                                    (+ (posn-y old-pos) y-velocity))))
                            ((draw-solid-ellipse w)
                             new-pos
                             ball-size
                             ball-size)
                              
                            (loop new-time new-pos x-velocity y-velocity))))))))))
        (begin
          (close-viewport w)
          (close-graphics)))))

;;; Draw lines, whose x/y coordinates reflect all the factors of n,
;;; where n is the current time in seconds, mod 50.
;;; Uses busy-waiting to wait for the second to increment.
(define (draw-line-mult-by-time)
  (open-graphics)
  (let ((w (open-viewport "draw-line-mult-by-time" horiz vert)))
    (dynamic-wind
        void
        (let loop ((old-time (modulo (current-seconds) 50)))
          (let ((time (modulo (current-seconds) 50)))
            (if (= time old-time)
                (loop time)
                (let ((ans (run* (q)
                             (fresh (a b)
                               (== (list a b) q)
                               (z/assert `(and (>= ,a 0) (>= ,b 0)))
                               (z/assert `(and (<= ,a 60) (<= ,b 60)))
                               (z/assert `(= (* ,a ,b) ,time))))))
                  (let ((factor-ls ans))
                    (displayln factor-ls)
                    ((clear-viewport w))
                    ((draw-string w) (make-posn horiz-center vert-center) (number->string time))
                    (for-each
                      (lambda (x/y)
                        ((draw-line w)
                         (make-posn 0 0)
                         (make-posn (+ 0 (* (car x/y) 10))
                                    (+ 0 (* (cadr x/y) 10)))))
                      factor-ls)
                    (loop time))))))
        (begin
          (close-viewport w)
          (close-graphics)))))
