;; INF2810 - Innlevering 1a
;; Brukernavn: evgeniag

;; Oppgave 1
;; ---------

;;Evalueringsreglene:
;;To evaluate a combination, do the following:
;;1.  Evaluate the subexpressions of the combination.
;;2.  Apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands).

;;(a)
(* (+ 4 2) 5)
;;30 fordi: (* (+ 4 2) 5) -> (* 6 5) -> 30
 
;;(b)
;;(* (+ 4 2) (5))
;;Feil:. . application: not a procedure;
;;Fordi kombinasjon (5) må begynne med en operatør

;;(c)
;;(* (4 + 2) 5)
;;Feil:. . application: not a procedure;
;;Fordi kombinasjon (4 + 2) må begynne med en operatør, dvs (+ 4 2)

;;(d) > (define bar (/ 42 2))
;;bar -> (/ 42 2) -> 21

;;(e) > (- bar 11) -> (- 21 11) -> 10

;;(f) > (/ (* bar 3 4 1) bar) -> (/ (* 21 3 4 1) 21) -> 12

;;2. 