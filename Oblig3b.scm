;; INF2810 - Innlevering 3b
;; Brukernavn: evgeniag

(load "evaluator.scm")

;;Oppgave 1

(set! the-global-environment (setup-environment))

;;a
;;(define (foo cond else)
;;  (cond ((= cond 2) 0)
;;        (else (else cond))))

;;(define cond 3)
;;(define (else x) (/ x 2))
;;(define (square x) (* x x))


;;(foo 2 square) -> 0
;;forste cond (og else) skilles fra variablene i evaluatoren
;;ved hjelp av cond? og cond-clauses funksjonene
;; Selv om "cond" og "else" er definert i globale omgivelser,
;; innenfor foo er 'cond'= 2, og 'else'= 'square'
;; fordi denne koden kjøres i sine egne foo-omgivelser,
;; der cond og else er bindet til de verdiene funksjonen ble kalt med

;;(foo 4 square) -> 16
;; samme her


;;(cond ((= cond 2) 0)
;;(else (else 4))) -> 2
;; Har er det globale omgivelser som gjelder,
;; dvs 'cond' = 3 og 'else'= lamda (x) (/ x 2)

;;Oppgave 2
;;a
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
;; ny primitive: 1+
        (list '1+
              (lambda (x) (+ 1 x)))

;; ny primitive: 1-        
        (list '1-
              (lambda (x) (- x 1)))
        ))

;;b
(define (install-primitive! name func)
  (define-variable! name (list 'primitive func) the-global-environment)
  (display 'installed!))

(install-primitive! 'square (lambda (x) (* x x)))


;;Oppgave 3
;;a
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; 3a
        ((or? exp) #t) ;; 3a
        (else #f)))

(define (and? exp) (tagged-list? exp 'and)) ;;3a
(define (or? exp) (tagged-list? exp 'or))   ;;3a 


(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env)) ;;3a
        ((or? exp) (eval-or exp env)))) ;;3a

;;To nye prosedyrer:
;;3a
(define (eval-and exp env)
  (if (null? (cdr exp))
      #t
      (if (false? (mc-eval (cadr exp) env))
          #f
          (eval-and (cons 'and (cddr exp)) env))))


;;3a
(define (eval-or exp env)
  (if (null? (cdr exp))
      #f
      (if (true? (mc-eval (cadr exp) env))
          #t
          (eval-or (cons 'or (cddr exp)) env))))


;;b
;;hele prosyderen eval-if er omskrevet
(define (eval-if exp env)  
  (if (else? exp)
      (mc-eval (if-alternative exp) env)    
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (eval-if (elseif-part exp) env))))

(define (if-consequent exp) (cadddr exp)) ;;3b caddr -> cadddr

(define (else? exp) (tagged-list? exp 'else)) ;;3b ny prosedyre
  
(define (if-alternative exp) ;;3b omskrevet
  (cadr exp))

(define (elseif-part exp) (cddddr exp)) ;;3b ny prosedyre

;;c
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ((and? exp) #t) ;; 3a
        ((or? exp) #t) ;; 3a
        ((let? exp) #t) ;; 3c
        (else #f)))

(define (let? exp) (tagged-list? exp 'let)) ;;3c ny prosedyre

(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ((and? exp) (eval-and exp env)) ;;3a
        ((or? exp) (eval-or exp env)) ;;3a
        ((let? exp) (mc-eval (let->lambda exp) env)))) ;;3c

;;2 nye prosedyrer:
(define (let->lambda exp)
  (let ((bindings (cadr exp))
        (body (caddr exp)))
    (make-lamda bindings body)))

(define (make-lamda bindings body)
  (cons (list 'lambda (map car bindings) body) (map cadr bindings)))





