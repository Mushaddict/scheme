#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace racket
;#lang racket
(provide (all-defined-out))
(require "hw4-util.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Exercise 1
(define (s:subst exp var val)
  (cond
    [(s:number? exp) exp]
    [(s:variable? exp)
     (cond
       [(equal? exp var) val]
       [else exp])]
    [(s:lambda? exp)
     (cond
       [(equal? (s:lambda-param1 exp) var) exp]
       [else (s:lambda (s:lambda-params exp)
                       (list (s:subst (s:lambda-body1 exp) var val)))])]
    [(s:apply? exp)
     (s:apply (s:subst (s:apply-func exp) var val)
              (list (s:subst (s:apply-arg1 exp) var val)))]))


;; Exercise 2
(define (s:eval subst exp)
  (cond [(s:value? exp) exp]
        [else
         (s:eval subst (s:subst (s:lambda-body1 (s:eval subst (s:apply-func exp)))
                                (s:lambda-param1 (s:eval subst (s:apply-func exp)))
                                (s:eval subst (s:apply-arg1 exp))))]))

;; Exercise 3
(define (e:eval env exp)
  (cond [(e:value? exp) exp]
        [(e:variable? exp)
         (cond [(and (hash? env) (hash-has-key? env exp)) (hash-ref env exp)]
               [else exp])]
        [(e:lambda? exp) (e:closure env exp)]
        [(e:apply? exp) (begin
         (define ef (e:eval env (e:apply-func exp)))
         (define ea (e:eval env (e:apply-arg1 exp)))
         (define x (e:lambda-param1 (e:apply-func exp)))
         (define eb (e:closure-env ef))
         (define ht (hash-set eb x ea))
         (e:closure ht (e:lambda-body1 (e:apply-func exp))))]))
         
        

;; Exercise 4 (Manually graded)
#|
The situation where using lambda-Racket with environments is better than without is
the situation where we have a large and complicated hash table where it stores huge
amount of pairs inside. Using a table here has a better searching functions.
However, using lambda-Racket without environments happens in the siutaion where we
only have a few amount of data, espeically when we only have (let's say less than 5
pairs) of data. At this point, building a hash table is complicated than not building
it but use those pairs directly and show those pairs in the comments part. 
|#

;; Exercise 5 (Manually graded)
#|
benefits:
1. formal specification can help developers understand the certain algorithms straight
and clearly becuase they can communicate better if anywhere goes wrong. 
2. formal specificaiton can help save the space and let people see it clearly without
reading too much stuff that sometimes will be written too complicated. 
|#
