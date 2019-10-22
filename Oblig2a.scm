;; INF2810 - Innlevering 2a
;; Brukernavn: evgeniag

(load "huffman.scm")

;;Oppgave 1
;;---------

;;a
(define (p-cons x y)
(lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;b
(define foo 42)

;;evaluerer til 'different
((lambda (foo x)
  (if (= x foo)
    'same
    'different))
 5 foo)

;;evaluerer til (towel (42 towel))
((lambda (bar baz)
   ((lambda (bar foo)
    (list foo bar))
   (list bar baz) baz))
 foo 'towel)


;;c
(define (infix-eval exp)
  ((cadr exp) (car exp) (caddr exp)))

;;d
;;Det er fordi (list ..) lager en liste av verdier, mens '(...) lager en liste av symboler som ikke blir evaluert
;;Dvs / oppfates ikke som en prosedyre, men som et symbol i dette tilfellet

;;Oppgave 2
;;---------

;;a
(define (member? pred elem items)
  (cond ((null? items) #f)
        ((pred elem (car items)) #t)
        (else (member?  pred elem (cdr items)))))

;;b
;;Det er fordi vi vil skille mellom to tilfeller av kallet paa decode:
;; 1 - Naar vi starter å dekodere et nytt symbol. Da vil vi ha hele treet i variabelen "tree"
;;     (decode-1 (cdr bits) tree))
;; 2 - Naar vi allerede er i gang med dekodering. Da vil vi bruke resten av treet.
;;     (decode-1 (cdr bits) next-branch))
;;dvs for aa kunne ha tilgang til hele treet fra indre rekursive kall

;;c
(define (decode-hale-rekursiv bits tree)
  (define (decode-1 bits current-branch out)
    (if (null? bits)
        out
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-1 (cdr bits)
                        tree
                        (append out
                                (cons (symbol-leaf next-branch) '())))
              (decode-1 (cdr bits) next-branch out)))))
  (decode-1 bits tree '()))

;;d
;;(ninjas fight ninjas by night)

;;e
(define (encode message tree)
  (define (encode-1 message current-branch)
    (if (null? message)
        '()
        (let ((next-branch (choose-branch-encode (car message) current-branch))
          (bit (calc-bit (car message) current-branch)))
      (if (leaf? next-branch)
          (cons bit (encode-1 (cdr message) tree))
          (cons bit (encode-1 message next-branch))))))
  (encode-1 message tree))


(define (choose-branch-encode symbol tree)
  (if (= 0 (calc-bit symbol tree))
      (left-branch tree)
      (right-branch tree)))

(define (calc-bit symbol tree)
  (let ((symbols-left (symbols (left-branch tree))))
  (if (member? eq? symbol symbols-left)
      0
      1)))

;;f
(define (grow-huffman-tree items)
    (define (grow-huffman-tree-from-leaves leaf-set)
    (if (= 1 (length leaf-set))
        (car leaf-set)
        (grow-huffman-tree-from-leaves
         (adjoin-set
          (make-code-tree (car leaf-set) (cadr leaf-set))
          (cddr leaf-set)))))
    (grow-huffman-tree-from-leaves (make-leaf-set items)))

;;g
 (define freqs '((samurais 57) (ninjas 20) (fight 45)
                 (night 12) (hide 3) (in 2)
                 (ambush 2) (defeat 1) (the 5)
                 (sword 4) (by 12) (assassin 1)
                 (river 2) (forest 1) (wait 1) (poison 1)))

(define codebook (grow-huffman-tree freqs))
(define message '(ninjas fight ninjas
                         fight ninjas
                         ninjas fight samurais
                         samurais fight
                         samurais fight ninjas
                         ninjas fight by night))

;;Hvor mange bits bruker det paa aa kode meldingen
;;Svaret: 43
(length (encode message codebook))

;; gjennomsnittlige lengden paa hvert kodeord som brukes:
;;Svaret: 2 9/17 
(/ (length (encode message codebook)) (length message))

;;Vi har 16 symboler i alfabetet
;;dvs vi trenger 4 bit for aa kode hvert ord i fastlengde
;;For aa kode 17 symboler trenger vi 17*4 = 68 bits
(* 17 4)


;;h
(define (huffman-leaves tree)
  (cond ((leaf? tree)  (list (list (symbol-leaf tree) (weight-leaf tree))))
        (else (append (huffman-leaves (left-branch tree)) (huffman-leaves (right-branch tree))))))
;;test
;;(huffman-leaves (grow-huffman-tree (huffman-leaves codebook)))

;;i
(define (expected-code-length tree)
  (let ((total-freq (weight tree)))
    (define (iter length branch)    
      (if (leaf? branch)
        (* (/ (weight branch) total-freq) length)
        (+ (iter (+ 1 length) (left-branch branch)) (iter (+ 1 length) (right-branch branch)))        
      ))
    (+ (iter 1 (left-branch tree)) (iter 1 (right-branch tree)))))


'(expected-code-length sample-tree:)
(expected-code-length sample-tree)

