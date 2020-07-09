;#lang errortrace racket
#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at:

   https://www.umb.edu/life_on_campus/dean_of_students/student_conduct

|#
(require racket/set)
(require "hw6-util.rkt")
(provide frame-refs mem-mark mem-sweep mlist mapply)

;;;;;;;;;;;;;;;
;; Exercise 1

(define (frame-values-refs values refset)
  (if (equal? values '()) refset
      (let ([head (car values)]
             [tail (cdr values)])
       (cond
         [(d:lambda? head) (frame-values-refs tail refset)]
         [(d:apply? head) (frame-values-refs tail refset)]
         [(d:number? head) (frame-values-refs tail refset)]
         [(d:variable? head) (frame-values-refs tail refset)]
         [(d:define? head) (frame-values-refs tail refset)]
         [(d:closure? head)
          (frame-values-refs tail (set-add refset (d:closure-env head)) )]
         [(d:void? head) (frame-values-refs tail refset)]
         [else (error "Unsupported term: " head)]))
 )
)

(define/contract (frame-refs frm)
  (-> frame? set?)
  (frame-values-refs (frame-values frm)
  (if (equal? (frame-parent frm) #f) (set) (set (frame-parent frm)))) )

;;;;;;;;;;;;;;;
;; Exercise 2

(define (mem-mark-once contained mem handleset)
  (if (equal? handleset '()) '()
      (append (list (car handleset)) (set->list (contained (heap-get mem (car handleset)))) (mem-mark-once contained mem (cdr handleset)))
  )
)

(define (mem-mark-equal l1 l2)
  (equal? (length (set->list (list->set l1)))  (length (set->list (list->set l2))))
)

(define (mem-mark-muti contained mem prev)
  (let ([curr (mem-mark-once contained mem prev)])
    (if (mem-mark-equal prev curr) curr
        (mem-mark-muti contained mem curr)
        )
  )
)

(define/contract (mem-mark contained mem env)
  (-> (-> any/c set?) heap? handle? set?)
  (list->set (mem-mark-muti contained mem (list env))))

;;;;;;;;;;;;;;;
;; Exercise 3

(define/contract (mem-sweep mem to-keep)
  (-> heap? set? heap?)
  (heap-filter (lambda (k v) (set-member? to-keep k)) mem))

;;;;;;;;;;;;;;;
;; Exercise 4

(define (mlist-aux bind pure args xs)
  (if (equal? args '()) (pure (reverse xs))
      (do
        x <- (car args)
        (mlist-aux bind pure (cdr args) (cons x xs))
      )
  )
)

(define (mlist bind pure args)
  (mlist-aux bind pure args '()))

;;;;;;;;;;;;;;;
;; Exercise 5

(define (mapply-aux bind pure f args xs)
  (if (equal? args '()) (pure (apply f (reverse xs)))
      (do
        x <- (car args)
        (mapply-aux bind pure f (cdr args) (cons x xs))
      )
  )
)

(define (mapply bind pure f . args)
  (mapply-aux bind pure f args '()))

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
PLEASE REPLACE THIS TEXT BY YOU ANSWER.
YOU MAY USE MULTIPLE LINES.
Soundness is affected.
For a memory whose reference count is not 0, the count may be 0 due to the overflow of the count, which will be reclaimed.
Completeness is affected.
For a memory with a reference count of 0, the count may not be 0 due to the faulty of the reference count algorithm, so it will not be reclaimed.
|#
