;; INF2810 - Innlevering 2b
;; Brukernavn: evgeniag

(load "prekode2b.scm")

;;Oppgave 1
;;---------

;;a
(define (make-counter)
  (let ((count 0))
    (lambda () 
      (set! count (+ 1 count))
      count)))

;;b
;;i pdf 1b


;;Oppgave 2
;;---------

;;a
(define (make-stack items)
  (define (pop!)
    (if (null? items)
        "The stack is empty"
        (begin (set! items (cdr items))
               "")))

  (define (push! . args)
      (define (build-lifo newitems)
         (if (null? newitems)
             items
             (cons (car newitems) (build-lifo (cdr newitems)))))
      (set! items (build-lifo (reverse args))))  

  (define (stack)
    items)
  
  (define (dispatch message . args)
    (cond ((eq? message 'pop!) (pop!))
          ((eq? message 'stack) (stack))
          ((eq? message 'push!) (apply push! args))
          (else "Unknown request")))
  dispatch)

;;b
(define (stack st)
  (st 'stack))
(define (pop! st)
  (st 'pop!))
(define (push! st . args)
   (apply st (cons 'push! args)))

;;Oppgave 3
;;---------

;;a,b
;;i pdf 3a og 3b

;;c
;;bar er ikke en liste fordi den ikke har '() paa slutten
;;bah tilsvarer kravene til en liste (slutter paa '())

;;Oppgave 4
;;---------

;;a + b
(define (memoize-version f)
  (let ((table (make-table)))
    (lambda args
      (let ((previously-computed-result (lookup args table)))
        (or previously-computed-result
            (let ((result (apply f args)))
              (insert! args result table)
              result))))))


(define (make-mem)
  (let ((table (make-table)))

  (define (dispatch message func)
    (cond ((eq? message 'memoize) (memoize func))
          ((eq? message 'unmemoize) (unmemoize func))
          ;;((eq? message 'table) (display-table)) ;;for test
          (else (display "Unknown request"))))

   (define (memoize func)
     (let ((result (memoize-version func)))
          (insert! result func table)
          result))

    (define (unmemoize func)
      (let ((original-func (lookup func table)))
          (or original-func
              (display "Procedure hasn't been memoized")
              func)))

    (define (display-table)
      (display table))

    dispatch))
        
       
(define mem (make-mem))

;;c
;;Det er fordi i (define mem-fib (mem 'memoize fib)) endrer vi ikke bindingen til fib
;; mem-fib fortsetter aa bruke vanlig fib innerst
;;I (set! fib (mem 'memoize fib)) endrer vi bindingen til fib-en,
;;saa fib-en som brukes innerst i prosedyrens kropp er ogsaa memoizet


