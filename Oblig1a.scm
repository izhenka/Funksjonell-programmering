;; INF2810 - Innlevering 1a
;; Brukernavn: evgeniag

;;Oppgave 1
;;---------

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

;;(d)
(define bar (/ 42 2))
bar
;;21 fordi: bar -> (/ 42 2) -> 21

;;(e)
(- bar 11)
;;10 fordi: (- 21 11) -> 10

;;(f)
(/ (* bar 3 4 1) bar)
;;12 fordi: (/ (* bar 3 4 1) bar) -> (/ (* 21 3 4 1) 21) -> 12


;;Oppgave 2
;;---------

;;a
(or (= 1 2)
"piff!"
"paff!"
(zero? (1 - 1)))
;; Evalueres til "piff" fordi "or" evalueres til det første argumentet som ikke er #f (ellers er resultatet #f).
;;(= 1 2) -> #f
;; "piff!" er ikke #f, så det blir svaret. Resterende argumenter evalueres ikke.

(and (= 1 2)
"piff!"
"paff!"
(zero? (1 - 1)))
;; Evalueres til #f fordi "and" evalueres til #f med en gang hvis en av de argumentene er #f (ellers evalueres det til det siste argumentet).
;;(= 1 2) -> #f så det blir svaret. Resterende argumenter evalueres ikke.


(if (positive? 42)
"poff!"
(i-am-undefined))
;; Evalueres til "poff" fordi for å evaluere et if-uttrykk begynner interpreter med å evaluere predicate (positive? 42) -> #t
;; Hvis predicate er sant, så evaluerer interpreter consequent ("poff!") og returnerer dens verdi ("poff!").

;;b
(define (sign t)
  (cond ((zero? t) 0)
        ((positive? t) 1)
        (else -1)))

(define (sign2 t)
  (if (zero? t)
      0
      (if (positive? t)
          1
          -1)))

;;c
(define (sign3 t)
  (or (and (positive? t) 1)
      (and (negative? t) -1)
      0))

;;Oppgave 3
;;---------

;;a
(define (add1 t)
  (+ t 1))

(define (sub1 t)
  (- t 1))

;;b
(define (plus a b)
  ( if (zero? a)
       b
       (plus (sub1 a) (add1 b))))

;;c
;;Prosedyren definert i oppgave b gir opphav til en iterativ prosess
;;
;;Utvikling av prosessen:
;;(plus 4 1)
;;(plus 3 2)
;;(plus 2 3)
;;(plus 1 4)
;;(plus 0 5)
;;5
;;
;;Det vil si at vi ikke har en kjede av ventende kall (som kjennetegner en rekursiv prosess).
;;Prosessen kan til enhver tid oppsummeres med et gitt antall tilstandsvariabler
;;Derfor er dette en iterativ prosess.

;;Prosedyre med en rekursiv prosess:
(define (plus2 a b)
  ( if (zero? a)
       b
       (+ 1 (plus (sub1 a)  b))))

;;d
;(define (power-close-to b n)
;  (power-iter b n 1))

;(define (power-iter b n e)
;  (if (> (expt b e) n)
;      e
;      (power-iter b n (+ 1 e))))

;;forenklet versjon med blokk-struktur:
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))

  (power-iter 1))

;;Vi trenger ikke å sende inn b og n ekslisitt i power-iter, 
;;fordi power-iter er allerede i scope av b og n


;;e

;;(define (fib n)
;;  (fib-iter 1 0 n))

;;(define (fib-iter a b count)
;;  (if (= count 0)
;;      b
;;     (fib-iter (+ a b) a (- count 1))))

;;Versjonen med blokk-struktur:
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

  (fib-iter 1 0 n))

;; Her er det ikke mulig å forenkle prosedyren fib-iter ved å fjerne parameteren 'count' (og bruke felles variabel 'n' istedenfor) .
;; Fordi 'count' er en teller-variabel som går ned i hver kall på 'fib-iter'. Den stemmer ikke med n.
