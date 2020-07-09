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
;#lang errortrace racket
#lang racket
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES

;; Exercise 1
; (closure envf expl)
(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
   (cond
    [(d:value? exp) (eff mem exp)]
    [(d:variable? exp) (eff mem (environ-get mem env exp))]
    [(d:apply? exp)
      (define ef (d:apply-func exp))
      (define ea (car (d:apply-args exp)))
      (define sr (d:eval-exp mem env ef))
      (define memm (eff-state sr))
      (define clos (eff-result sr))
      (define envf (d:closure-env clos))
      (define lmda (d:closure-decl clos))
      (define x (car (d:lambda-params lmda)))
      (define tb (d:lambda-body lmda))
      (define emb0 (d:eval-exp memm env ea))
      (define va (eff-result emb0))
      (define mem2 (eff-state emb0))
      (define emb (environ-push mem2 envf x va))
      (define memb (eff-state emb))
      (define envb (eff-result emb))
      (define emb2 (d:eval-term memb envb tb))
      (define vb (eff-result emb2))
      (define membb (eff-state emb2))
      (eff membb vb)]
    [(d:lambda? exp)
     (eff mem (d:closure env exp))]
    [else (print exp)]
    )
)


;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
  (cond
    [(d:expression? term) (d:eval-exp mem env term)]
    [(d:define? term)

     (let ([v (eff-result (d:eval-exp mem env (d:define-body term)))])
       (eff (environ-put mem env (d:define-var term) v) (d:void))
       )
     ]
    [(d:seq? term)
     (define sr (d:eval-term mem env (d:seq-fst term)))
     (define memm (eff-state sr))
     (define envv (eff-result sr))
     (define sr2 (d:eval-term memm env (d:seq-snd term)))
     (define memmm (eff-state sr2))
     (define result (eff-result sr2))
     (eff memmm result)]
  )
)

;; Exercise 3 (Manually graded)
#|
PLEASE REPLACE THIS TEXT BY YOUR ANSWER.
YOU MAY USE MULTIPLE LINES.
λD is dynamically scoped,  Racket is lexically scoped.
Exmaple:
(let (x 5)
(let (f (lambda (y) (+ x y))
(let (x 0) 
(f 6)
)))
In Racket, it is evaluated to 11.
In λD, it is evaluated to 6.
|#
