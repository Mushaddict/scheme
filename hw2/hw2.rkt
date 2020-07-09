#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1.a: Read-write cell
;; Solution has 3 lines.
(define (rw-cell x)
  (define (dispatch m)
    (cond ((equal? m (list)) x)
          (else (rw-cell (first m)))))
  dispatch)
;;(define c (rw-cell 10))

;; Exercise 1.b: Read-only cell
;; Solution has 4 lines.
(define (ro-cell x)
  (define (dispatch m)
    (cond ((equal? m (list)) x)
          (else (ro-cell x))))
  dispatch)

;; Exercise 2: Interperse
;; Solution has 11 lines.
;;(define (intersperse l v)
;;  (cond ((empty? l) '())
;;        ((empty? v) l)
;;        (else (cons (list (first l) v) (intersperse (rest l) v)))))
(define (intersperse l v)
  (cond ((empty? l) '())
        (else (append
               (list (first l))
               (cond ((empty? (rest l)) '())
                     (else (list v)))
               (intersperse (rest l) v)))))


(define c (intersperse (list 1 2 3 4) 0))

;; Exercise 3.a: Generic find
;; Solution has 7 lines.
(define (find pred l)
  (define (check l idx)
    (cond ((empty? l) #f);;basecase
          ((pred idx (first l)) (cons idx (first l)))  ;;if true, cons 
          (else (check (rest l) (+ 1 idx)))))    ;;else check the rest list
  (check l 0))


;; Exercise 3.b: Member using find
;; Solution has 3 lines.
(define (member x l)
  (cond ((empty? l) #f) ;;basecase
        ((equal? (first l) x) #t);;compare the first elemnt
        (else (member x (rest l)))))
#|(define (member x l)
  (cond ((empty? l) #f)
        ((equal? x (find (lambda (idx elem) #t) l)) #t)
        (else (member x (rest l)))))|#



;; Exercise 3.c: index-of using find
;; Solution has 4 lines.
(define (index-of l x)
  (define (return-idx l index)
    (cond ((empty? l) #f)
          ((equal? (first l) x) index)
          (else
           (+ 1 index);; index++
           (return-idx (rest l) (+ 1 index)))))
  (return-idx l 0))


;; Exercise 4: uncurry, tail-recursive
;; Solution has 8 lines.
(define (uncurry f)
  (define (dispatch m)
    (define (uncur func l)
      (cond ((= 1 (length l)) (func (first l)))
            (else (uncur (func (first l)) (rest l)))))
    (cond ((empty? m) (f))
          (else (uncur f m))))
  dispatch)


;; Exercise 5: Parse a quoted AST
;; Solution has 26 lines.
(define (parse-ast node)
  
  (define (make-define-func node)
    (r:define
     (parse-ast (first (second node)))
     (r:lambda (map parse-ast (cdr (second node)))
               (map parse-ast (cdr (cdr node))))))
  
  (define (make-define-basic node)
    (r:define
     (parse-ast (second node))
     (parse-ast (third node))))
  
  (define (make-lambda node)
    (r:lambda
     (map parse-ast (car (cdr node)))
     (map parse-ast (cdr (cdr node)))))
  
  (define (make-apply node)
    (r:apply
     (parse-ast (first node))
     (map parse-ast (cdr (cdr node)))))
  
  (define (make-number node)
    (r:number node))
  
  (define (make-variable node)
    (r:variable node))

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))

(check-equal? (parse-ast 'x) (r:variable 'x))
(check-equal? (parse-ast '10) (r:number 10))
(check-equal?
  (parse-ast '(lambda (x) x))
  (r:lambda (list (r:variable 'x)) (list (r:variable 'x))))

