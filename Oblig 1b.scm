;; INF2810 - Innlevering 1b
;; Brukernavn: evgeniag

;;Oppgave 1
;;---------

(define bar 'bar)
;;(f) (0 42 #t bar)
(define 1f (list 0 42 #t bar))
(car (cdr 1f)) ;; <- svar

;;(g) ((0 42) (#t bar))
(define 1g (list (cons 0 42) (cons #t bar)))
(cdr (car 1g)) ;; <- svar

;;(h) ((0) (42 #t) (bar))
(define 1h (list 0 (cons 42 #t) bar))
(car (car (cdr 1h))) ;; <- svar

;;(i)
(list (cons 0 42) (cons #t bar))


;;Oppgave 2
;;---------

;;a
(define (length2 items)
  (define (iter res items)
    (if (null? items)
        res
        (iter (+ 1 res) (cdr items))))
  (iter 0 items))

;;b
(define (reduce-reverse proc init items)
  (define (iter out items)
    (if (null? items)
        out
        (iter (proc (car items) out)
              (cdr items))))
  (iter init items))
;;Prosedyren er halerekursiv og gir opphav til en iterativ prosess
;;Utvikling av prosessen for (reduce-reverse cons '() '(1 2 3)):
;;(iter '() '(1 2 3))
;;(iter '(1) '(2 3))
;;(iter '(2 1) '(3))
;;(iter '(3 2 1) '())
;;(3 2 1)
;;Det vil si at vi ikke har en kjede av ventende kall.
;;Prosessen kan til enhver tid oppsummeres med et gitt antall tilstandsvariabler
;;Derfor er dette en iterativ prosess.

;;c.1
(define (all? pred items)
  (cond  ((null? items) #t)
         ((pred (car items)) (all? pred (cdr items)))
         (else #f)))

;;c.2
(all? (lambda (x)
        (if (> x 10)
            #f
            #t))
      '(1 2 3 4 50))

;;d
(define (nth index items)
  (if (zero? index)
      (car items)
      (nth (- index 1) (cdr items))))

;;e
(define (where element items)
  (define (iter index items)
    (cond ((null? items) #f)
          ((= element (car items)) index)
          (else (iter (+ 1 index) (cdr items)))

     ))
  (iter 0 items))

;;f
(define (map2 proc items1 items2)
  (if (or (null? items1)
          (null? items2))
      '()
      (cons (proc (car items1) (car items2))
            (map2 proc (cdr items1) (cdr items2)))))


;;g
(map2 (lambda (x1 x2) (/ (+ x1 x2) 2)) '(1 2 3 4) '(3 4 5))

;;h
(define (both? pred)
  (lambda (x y)
    (and (pred x) (pred y))))

;;i
(define (self proc)
  (lambda (x) (proc x x)))
