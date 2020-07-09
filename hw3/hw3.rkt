;;#lang errortrace racket
#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;; Exercise 1.a
(define p:empty
  (delay '()))

;; Exercise 1.b
(define (p:empty? p)
  (if (empty? (force p)) #t #f))

;;Exercise 1.c
;;x: any value, l: promise list
;(define (p:cons x l)
;  (cond ((p:empty? x) (cons '() (delay l)))
;        (else
;         (delay (cons x (delay l))))))

;;It is possible to construct a new promise list such that x is
;;the head of the resulting promise list, l is the tail of the promise list,
;;and x is not evaluated

;(define l1 (cons 1 (thunk (cons 2 (thunk (cons 3 '()))))))
;(check-equal? '() (p:first (p:cons '() l1)))
;(check-equal? 3 (p:first (p:cons 3 l1)))
;(check-equal? l1 (force (p:rest (p:cons 3 l1))))


;; Exercise 1.d
(define (p:first l)
  (car (force l)))

;; Exercise 1.e
(define (p:rest l)
  (cdr (force l)))


;; Exercise 1.f
(define (p:append l1 l2)
  (cond ((p:empty? l1) l2)
        (else (delay (cons (p:first l1)
                           (p:append (p:rest l1) l2))))))

(define l1 (cons 1 (thunk (cons 2 (thunk (cons 3 '()))))))
(define l2 (cons 4 (thunk (cons 5 (thunk (cons 6 7))))))

;; Exercise 2.a
;; Auxiliary functions
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))
#|
(define (bst->list self)
  (cond [(empty? self) self]
        [else
         (append
           (bst->list (tree-left self))
           (cons (tree-value self)
                 (bst->list (tree-right self))))]))
|#
(define (bst->p:list self)
  (cond [(p:empty? self) p:empty]
        [else
         (p:append
          (bst->p:list (tree-left self))
          (delay (cons (tree-value self)
                       (bst->p:list (tree-right self)))))]))

;;Exercise 2.b
;;eager evaluation is to take the best option in current situation.
;;lazy evaluation is to let the rest of the list performs later, while running
;;the current option.
;;In this case : (define a (p:append l1 l2)) where we have defined l1 l2 to be
;;two promise lists in up front
;;The Eager Evaluation here is to check if the first value is null, if not, it
;;will append the current node to the rest of the node. Thus, it will perform:
;;--> (append (first l1) (rest l1)) to l2
;;This is also what the Lazy Evaluation does, because in scheme, we can use delay
;;and force to make the promsise list runs later, which will also performs like:
;;--> (cons (first 1 (delay (cons (second l1) (delay (cons (third l1)).....(l2)
;;In this case, the lazy evalutaion will outperform the same algothrim as the
;;eager evaluation does. 

;; Exercise 3
;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))

(define (stream-foldl f a s)
  (cond ((null? s) a)
        (else
         (cons a
               (thunk (stream-foldl f (f (stream-get s) a) (stream-next s)))))))

;; Exercise 4
(define (stream-skip n s)
  (cond ((= n 0) s)
        (else (stream-skip (- n 1) (stream-next s)))))

;; Exercise 5
(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym 'and) 'and]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    ;5.a data r:bool
    [(r:bool? exp) (r:bool-value exp)]
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones
    [(r:apply? exp)
     (apply (r:eval-exp (r:apply-func exp)) (map r:eval-exp (r:apply-args exp)))
     ;((r:eval-exp (r:apply-func exp))
      ;(r:eval-exp (first (r:apply-args exp)))
      ;(r:eval-exp (second (r:apply-args exp))))
     ]
    [else (error "Unknown expression:" exp)]))

(struct r:bool (value) #:transparent)
