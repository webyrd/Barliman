> (load "twenty-four-puzzle-smart.scm")
Testing "remove-one-elemento-a"
Testing "24-puzzle-refute-a"
Testing "24-puzzle-refute-b"
Testing "24-puzzle-refute-c"
Testing "24-puzzle-a-check-answer-a"
Testing "24-puzzle-a-check-answer-b"
Testing "24-puzzle-i"
(- 7 (- 7 (* 4 6))) [answer 1, 51 seconds total elapsed (wall time)]
(+ 4 (+ 6 (+ 7 7))) [answer 2, 88 seconds total elapsed (wall time)]
(* 4 (- 6 (- 7 7))) [answer 3, 89 seconds total elapsed (wall time)]
(+ 4 (+ 7 (+ 6 7))) [answer 4, 97 seconds total elapsed (wall time)]
(* 4 (/ 6 (/ 7 7))) [answer 5, 113 seconds total elapsed (wall time)]
(+ 6 (+ 4 (+ 7 7))) [answer 6, 118 seconds total elapsed (wall time)]
(* 4 (- 7 (- 7 6))) [answer 7, 122 seconds total elapsed (wall time)]
(+ 6 (+ 7 (+ 4 7))) [answer 8, 124 seconds total elapsed (wall time)]
(* 4 (- 7 (/ 7 6))) [answer 9, 127 seconds total elapsed (wall time)]
(* 6 (- 4 (- 7 7))) [answer 10, 127 seconds total elapsed (wall time)]
(* 4 (+ 6 (- 7 7))) [answer 11, 135 seconds total elapsed (wall time)]
(* 4 (+ 7 (- 6 7))) [answer 12, 144 seconds total elapsed (wall time)]
(* 4 (- (+ 6 7) 7)) [answer 13, 149 seconds total elapsed (wall time)]
(* 4 (* 6 (/ 7 7))) [answer 14, 151 seconds total elapsed (wall time)]
(* 6 (/ 4 (/ 7 7))) [answer 15, 156 seconds total elapsed (wall time)]
(* 4 (/ (* 6 7) 7)) [answer 16, 162 seconds total elapsed (wall time)]
(+ 7 (+ 4 (+ 6 7))) [answer 17, 168 seconds total elapsed (wall time)]
(* 6 (- 7 (- 7 4))) [answer 18, 168 seconds total elapsed (wall time)]
(+ 7 (+ 6 (+ 4 7))) [answer 19, 177 seconds total elapsed (wall time)]
(+ 7 (+ 7 (+ 4 6))) [answer 20, 181 seconds total elapsed (wall time)]
(* 6 (+ 4 (- 7 7))) [answer 21, 184 seconds total elapsed (wall time)]
(+ 7 (- (* 4 6) 7)) [answer 22, 185 seconds total elapsed (wall time)]
(* 6 (+ 7 (- 4 7))) [answer 23, 195 seconds total elapsed (wall time)]
(* 6 (- (+ 4 7) 7)) [answer 24, 200 seconds total elapsed (wall time)]
(* 6 (* 4 (/ 7 7))) [answer 25, 203 seconds total elapsed (wall time)]
(* 6 (/ (* 4 7) 7)) [answer 26, 221 seconds total elapsed (wall time)]
(- (* 4 6) (- 7 7)) [answer 27, 313 seconds total elapsed (wall time)]
(/ (* 4 6) (/ 7 7)) [answer 28, 326 seconds total elapsed (wall time)]
(+ (+ 4 6) (+ 7 7)) [answer 29, 337 seconds total elapsed (wall time)]
(+ (+ 4 7) (+ 6 7)) [answer 30, 377 seconds total elapsed (wall time)]
(/ (* 7 7) (- 6 4)) [answer 31, 403 seconds total elapsed (wall time)]
(+ (- 7 7) (* 4 6)) [answer 32, 431 seconds total elapsed (wall time)]
(- (+ 7 (* 4 6)) 7) [answer 33, 449 seconds total elapsed (wall time)]
(/ (* 4 (* 6 7)) 7) [answer 34, 459 seconds total elapsed (wall time)]
(* (* 4 6) (/ 7 7)) [answer 35, 466 seconds total elapsed (wall time)]
(+ (+ 6 7) (+ 4 7)) [answer 36, 474 seconds total elapsed (wall time)]
(+ (* 4 6) (- 7 7)) [answer 37, 485 seconds total elapsed (wall time)]
(/ (* 6 (* 4 7)) 7) [answer 38, 505 seconds total elapsed (wall time)]
(* (/ 7 7) (* 4 6)) [answer 39, 514 seconds total elapsed (wall time)]
(/ (* 7 (* 4 6)) 7) [answer 40, 531 seconds total elapsed (wall time)]
(+ (+ 7 7) (+ 4 6)) [answer 41, 552 seconds total elapsed (wall time)]
(time (test "24-puzzle-i" ...))
    213 collections
    18.568510187s elapsed cpu time, including 0.246819785s collecting
    660.911928000s elapsed real time, including 0.247851000s collecting
    1736281152 bytes allocated, including 1704807376 bytes reclaimed
Testing "24-puzzle-j"
(- 5 (- 1 (* 2 10))) [answer 1, 19 seconds total elapsed (wall time)]
(+ 5 (- (* 2 10) 1)) [answer 2, 239 seconds total elapsed (wall time)]
(+ (- 5 1) (* 2 10)) [answer 3, 466 seconds total elapsed (wall time)]
(- (* 2 10) (- 1 5)) [answer 4, 527 seconds total elapsed (wall time)]
(- (+ 5 (* 2 10)) 1) [answer 5, 610 seconds total elapsed (wall time)]
(- (* 5 (/ 10 2)) 1) [answer 6, 648 seconds total elapsed (wall time)]
(- (/ (* 5 10) 2) 1) [answer 7, 737 seconds total elapsed (wall time)]
(/ (- (* 5 10) 1) 2) [answer 8, 768 seconds total elapsed (wall time)]
(+ (* 2 10) (- 5 1)) [answer 9, 798 seconds total elapsed (wall time)]
(time (test "24-puzzle-j" ...))
    342 collections
    27.523576119s elapsed cpu time, including 0.382856637s collecting
    946.637250000s elapsed real time, including 0.384061000s collecting
    2782330272 bytes allocated, including 2804531344 bytes reclaimed
Testing "24-puzzle-k"
(* 3 (- 7 (- 8 9))) [answer 1, 123 seconds total elapsed (wall time)]
(* 3 (- 8 (/ 7 9))) [answer 2, 147 seconds total elapsed (wall time)]
(* 3 (- 9 (- 8 7))) [answer 3, 158 seconds total elapsed (wall time)]
(* 3 (- 9 (/ 8 7))) [answer 4, 168 seconds total elapsed (wall time)]
(* 3 (/ 8 (/ 9 7))) [answer 5, 175 seconds total elapsed (wall time)]
(* 3 (+ 7 (- 9 8))) [answer 6, 185 seconds total elapsed (wall time)]
(* 3 (+ 7 (/ 9 8))) [answer 7, 189 seconds total elapsed (wall time)]
(* 3 (+ 8 (/ 7 9))) [answer 8, 195 seconds total elapsed (wall time)]
(* 3 (+ 9 (- 7 8))) [answer 9, 199 seconds total elapsed (wall time)]
(* 3 (- (+ 7 9) 8)) [answer 10, 202 seconds total elapsed (wall time)]
(* 3 (* 8 (/ 9 7))) [answer 11, 207 seconds total elapsed (wall time)]
(* 8 (- 3 (/ 7 9))) [answer 12, 212 seconds total elapsed (wall time)]
(* 8 (/ 3 (/ 9 7))) [answer 13, 244 seconds total elapsed (wall time)]
(* 8 (+ 3 (/ 7 9))) [answer 14, 287 seconds total elapsed (wall time)]
(* 8 (* 3 (/ 9 7))) [answer 15, 303 seconds total elapsed (wall time)]
(* 8 (/ (* 3 9) 7)) [answer 16, 327 seconds total elapsed (wall time)]
(- (* 3 8) (/ 7 9)) [answer 17, 457 seconds total elapsed (wall time)]
(/ (* 3 8) (/ 9 7)) [answer 18, 485 seconds total elapsed (wall time)]
(+ (/ 7 9) (* 3 8)) [answer 19, 618 seconds total elapsed (wall time)]
(* (* 3 8) (/ 9 7)) [answer 20, 661 seconds total elapsed (wall time)]
(+ (* 3 8) (/ 7 9)) [answer 21, 707 seconds total elapsed (wall time)]
(* (/ 9 7) (* 3 8)) [answer 22, 778 seconds total elapsed (wall time)]
(time (test "24-puzzle-k" ...))
    347 collections
    27.472265573s elapsed cpu time, including 0.364303029s collecting
    975.619015000s elapsed real time, including 0.365402000s collecting
    2829103792 bytes allocated, including 2813804496 bytes reclaimed
Testing "24-puzzle-a-all-streaming"
(* 8 (+ 1 (+ 1 1))) [answer 1, 110 seconds total elapsed (wall time)]
(time (test "24-puzzle-a-all-streaming" ...))
    118 collections
    9.532118545s elapsed cpu time, including 0.131158945s collecting
    321.609404000s elapsed real time, including 0.131511000s collecting
    960717408 bytes allocated, including 984607760 bytes reclaimed
Testing "24-puzzle-g-all-streaming"
(+ 2 (+ 2 (+ 10 10))) [answer 1, 58 seconds total elapsed (wall time)]
(+ 2 (+ 10 (+ 2 10))) [answer 2, 63 seconds total elapsed (wall time)]
(+ 10 (+ 2 (+ 2 10))) [answer 3, 97 seconds total elapsed (wall time)]
(+ 10 (+ 10 (+ 2 2))) [answer 4, 112 seconds total elapsed (wall time)]
(+ 10 (+ 10 (* 2 2))) [answer 5, 113 seconds total elapsed (wall time)]
(+ (+ 2 2) (+ 10 10)) [answer 6, 199 seconds total elapsed (wall time)]
(+ (+ 2 10) (+ 2 10)) [answer 7, 218 seconds total elapsed (wall time)]
(+ (* 2 2) (+ 10 10)) [answer 8, 259 seconds total elapsed (wall time)]
(+ (+ 10 10) (+ 2 2)) [answer 9, 306 seconds total elapsed (wall time)]
(+ (+ 10 10) (* 2 2)) [answer 10, 306 seconds total elapsed (wall time)]
(time (test "24-puzzle-g-all-streaming" ...))
    130 collections
    10.535921492s elapsed cpu time, including 0.139548310s collecting
    380.691970000s elapsed real time, including 0.139940000s collecting
    1062536208 bytes allocated, including 1043558960 bytes reclaimed
Testing "24-puzzle-h-all-streaming"
(- 2 (- 2 (* 2 12))) [answer 1, 3 seconds total elapsed (wall time)]
(* 2 (- 2 (- 2 12))) [answer 2, 38 seconds total elapsed (wall time)]
(* 2 (- 12 (- 2 2))) [answer 3, 46 seconds total elapsed (wall time)]
(* 2 (/ 12 (/ 2 2))) [answer 4, 56 seconds total elapsed (wall time)]
(+ 2 (- (* 2 12) 2)) [answer 5, 57 seconds total elapsed (wall time)]
(* 2 (+ 2 (- 12 2))) [answer 6, 67 seconds total elapsed (wall time)]
(* 2 (- (+ 2 12) 2)) [answer 7, 71 seconds total elapsed (wall time)]
(* 2 (* 2 (/ 12 2))) [answer 8, 75 seconds total elapsed (wall time)]
(* 12 (- 2 (- 2 2))) [answer 9, 75 seconds total elapsed (wall time)]
(* 2 (+ 12 (- 2 2))) [answer 10, 76 seconds total elapsed (wall time)]
(* 2 (* 12 (/ 2 2))) [answer 11, 82 seconds total elapsed (wall time)]
(* 2 (/ (* 2 12) 2)) [answer 12, 87 seconds total elapsed (wall time)]
(* 12 (/ 2 (/ 2 2))) [answer 13, 90 seconds total elapsed (wall time)]
(* 12 (+ 2 (- 2 2))) [answer 14, 103 seconds total elapsed (wall time)]
(* 12 (- (+ 2 2) 2)) [answer 15, 105 seconds total elapsed (wall time)]
(+ (- 2 2) (* 2 12)) [answer 16, 105 seconds total elapsed (wall time)]
(* 12 (* 2 (/ 2 2))) [answer 17, 115 seconds total elapsed (wall time)]
(* 12 (- (* 2 2) 2)) [answer 18, 116 seconds total elapsed (wall time)]
(* 12 (/ (+ 2 2) 2)) [answer 19, 123 seconds total elapsed (wall time)]
(* 12 (/ (* 2 2) 2)) [answer 20, 130 seconds total elapsed (wall time)]
(* (/ 2 2) (* 2 12)) [answer 21, 145 seconds total elapsed (wall time)]
(- (* 2 12) (- 2 2)) [answer 22, 161 seconds total elapsed (wall time)]
(/ (* 2 12) (/ 2 2)) [answer 23, 167 seconds total elapsed (wall time)]
(* (+ 2 2) (/ 12 2)) [answer 24, 184 seconds total elapsed (wall time)]
(- (+ 2 (* 2 12)) 2) [answer 25, 220 seconds total elapsed (wall time)]
(* (* 2 2) (/ 12 2)) [answer 26, 243 seconds total elapsed (wall time)]
(/ (* 2 (* 2 12)) 2) [answer 27, 249 seconds total elapsed (wall time)]
(* (* 2 12) (/ 2 2)) [answer 28, 252 seconds total elapsed (wall time)]
(+ (* 2 12) (- 2 2)) [answer 29, 255 seconds total elapsed (wall time)]
(* (/ 12 2) (+ 2 2)) [answer 30, 265 seconds total elapsed (wall time)]
(* (/ 12 2) (* 2 2)) [answer 31, 266 seconds total elapsed (wall time)]
(/ (* 12 (+ 2 2)) 2) [answer 32, 277 seconds total elapsed (wall time)]
(/ (* 12 (* 2 2)) 2) [answer 33, 279 seconds total elapsed (wall time)]
(time (test "24-puzzle-h-all-streaming" ...))
    121 collections
    9.830120311s elapsed cpu time, including 0.126811788s collecting
    349.329273000s elapsed real time, including 0.127191000s collecting
    982461920 bytes allocated, including 1009536288 bytes reclaimed
> 
