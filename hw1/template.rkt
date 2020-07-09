#lang racket

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))
(require rackunit)

(define ex1
  (/
   (- (/ 7 2) 2)
   (* (* 3 5) 6)
   )
)


(define ex2
  (list
   (/
    (- (/ 7 2) 2)
    (* (* 3 5) 6)
   )
   (/
    (- 7/2 2)
    (* (* 3 5) 6)
   )
   (/
    3/2
    (* (* 3 5) 6)
   )
   (/
    3/2
    (* 15 6)
   )
   (/ 3/2 90)
   3/180
   ))

(define (ex3 x y)
  (>
   (- x (* y 12))
   (* (- 15 12) (- 14 y))))


;; Constructs a tree from two trees and a value
(define (tree left value right)
  (list left value right))
;; Constructs a tree with a single node
(define (tree-leaf value)
  (list '() value '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;test cases
;;(define left-node (tree-leaf 3))
;;(define right-node (tree-leaf 4))
;;(define bigTree (tree left-node 5 right-node))
;;'((() 3 ()) 5 (() 4 ()))



;; Accessors
(define (tree-left self)
  (first self))
(define (tree-value self)
  (second self))
(define (tree-right self)
  (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value)
  (list (tree-left self) value (tree-right self)))
(define (tree-set-left self left)
  (list left (tree-value self) (tree-right self)))
(define (tree-set-right self right)
  (list (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value)
  (cond ((null? self) (tree-leaf value))
        ((equal? value (tree-value self)) (tree-set-value self value))
        ((< value (tree-value self))
         (tree-set-left self (bst-insert (tree-left self) value)))
        (else (tree-set-right self (bst-insert (tree-right self) value)) )
        ))


;; lambda
;;(define (lambda? node)
;; (equal? (first node) 'lambda))

(define (lambda? node)
  (and
   (list? node)
   (equal? (first node) 'lambda)
   (>= (length node) 3)
   (list? (second node))
   (andmap symbol? (second node))
   (not (equal? '() (caddr node)))))

      

(define (lambda-params node)
  (second node))

(define (lambda-body node)
  (cddr node))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;personal test-case
(define code (quote (lambda (x) y)))

;; apply
(define (apply? l)
  (and
   (list? l)
   (>= (length l) 1)
   (not (equal? (first l) 'lambda))))

;;(check-false (apply? (quote (lambda (x) x))))
;;(check-true (apply? (quote (x y))))
       

(define (apply-func node)
  (first node))

;;(check-equal? 'x (apply-func (quote (x y))))
  
(define (apply-args node)
  (cdr node))

;;(check-equal? (list 'y) (apply-args (quote (x y))))
  

;; define
(define (define? node)
  (or
   (define-basic? node)
   (define-func? node)))

(define (define-basic? node)
  (and
   (list? node)
   (>= (length node) 3)
   (equal? (first node) 'define)
   (symbol? (second node))
   ))



(define (define-func? node)
  (and
   (list? node)
   (>= (length node) 3)
   (equal? (first node) 'define)
   (list? (second node))
   (and
    (not(empty? (second node)))
    (andmap symbol? (second node)))
  ))


