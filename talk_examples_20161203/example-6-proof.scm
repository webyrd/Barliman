;; Example 6: proof checker becomes a theorem prover

(define test
  (lambda ()

    ; Given: (A (A -> B) (B -> C))
    ; Prove: C

    (proof? '(C (A (A -> B) (B -> C))
               modus-ponens
               (((B -> C) (A (A -> B) (B -> C)) assumption ())
                 (B (A (A -> B) (B -> C))
                    modus-ponens
                    (((A -> B) (A (A -> B) (B -> C)) assumption ())
                      (A (A (A -> B) (B -> C)) assumption ()))))))))

;; HACK: We visualize the answer to our query by synthesizing the dead code R.
(define answer
  (lambda ()
    ,R))

(define member?
  (lambda (x ls)
    (cond
      ((null? ls) #f)
      ((equal? (car ls) x) #t)
      (else (member? x (cdr ls))))))

;; Proof checker for a logic with implication (->)
;; A proof has the following structure:

;; (,C ,A ,rule-name ,sub-proofs)

;; where...
;; ,C is a conclusion, and
;; ,A is a list of assumptions.

(define proof?
  (lambda (proof)
    (match proof

      ;; Use an assumption.
      ;; If ,A is in our list of assumptions then we prove it.
      (`(,A ,assms assumption ()) (member? A assms))

      ;; Prove the hypothesis to prove the conclusion.
      ;; If we prove (,A -> ,B) and ,A then we prove ,B.
      (`(,B ,assms modus-ponens
         (((,A -> ,B) ,assms ,r1 ,ants1)
          (,A ,assms ,r2 ,ants2)))
       (and (proof? `((,A -> ,B) ,assms ,r1 ,ants1))
            (proof? `(,A ,assms ,r2 ,ants2))))

      ;; Hypothetically prove a conclusion.
      ;; If we prove ,B after assuming ,A then we prove (,A -> ,B)
      (`((,A -> ,B) ,assms conditional
         ((,B (,A . ,assms) ,rule ,ants)))
         (proof? `(,B (,A . ,assms) ,rule ,ants))))))

;; Test Cases part 1: fast proof search
'(proof? '(C (A (A -> B) (B -> C)) . ,Q)) => R
(proof? '(C (A (A -> B) (B -> C)) . ,Q)) => #t


;; Test Cases part 2: more interesting theorems

; commutativity of hypotheses
; (((A & B) -> C) -> ((B & A) -> C))
; ((A -> (B -> C)) -> (B -> (A -> C)))
'((A -> (B -> C)) -> (B -> (A -> C))) => C
'(proof? '(,C () . ,Q)) => R
(proof? '(,C () . ,Q)) => #t

; contraposition
; ((A -> B) -> (~B -> ~A))
; ((A -> B) -> ((B -> #f) -> (A -> #f)))
'((A -> B) -> ((B -> #f) -> (A -> #f))) => C
'(proof? '(,C () . ,Q)) => R
(proof? '(,C () . ,Q)) => #t

; transitivity of ->
; Note: this is just a generalization of contraposition.
; (((A -> B) & (B -> C)) -> (A -> C))
; ((A -> B) -> ((B -> C) -> (A -> C)))
'((A -> B) -> ((B -> C) -> (A -> C))) => C
'(proof? '(,C () . ,Q)) => R
(proof? '(,C () . ,Q)) => #t

; commutativity of &
; ((A & B) -> (B & A))
; (~(~A | ~B) -> ~(~B | ~A))
; (~(A -> ~B) -> ~(B -> ~A))
; (((A -> (B -> C)) -> C) -> ((B -> (A -> C)) -> C))

;; This one takes about 3 minutes to prove.
'(((A -> (B -> C)) -> C) -> ((B -> (A -> C)) -> C)) => C
'(proof? '(,C () . ,Q)) => R
(proof? '(,C () . ,Q)) => #t
