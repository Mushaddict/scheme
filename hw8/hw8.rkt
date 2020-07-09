;#lang errortrace racket
#lang racket
(require "hw8-util.rkt")

(provide (all-defined-out))

;; Utility function that converts a variable into a string
;; Useful when translating from SimpleJS into LambdaJS
(define (mk-field x)
  (match x [(s:variable x) (k:string (symbol->string x))]))

;; Utility function that allocates a j:object.
;; (mk-object) allocates an empty object
;; (mk-object (cons "foo" (k:number 1)) (cons "bar" (j:variable 'x)))
;;  allocates an object with one field "foo" and one field "bar"
(define/contract (mk-object . args)
  (-> (cons/c string? j:expression?) ... j:alloc?)
  (define (on-elem pair)
    (cons (k:string (car pair)) (cdr pair)))
  (j:alloc (j:object (make-immutable-hash (map on-elem args)))))

;;;;;;;;;;;;;;;
;; Exercise 1

(define/contract (translate exp)
  (-> s:expression? j:expression?)
  ;(printf "translate: ~a\n" exp)
  (match exp
    [(? k:const? k) k]
    [(s:variable x) (j:variable x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))]
    [(s:load obj field) (j:get (j:deref (translate obj)) (translate (mk-field field)))]
    [(s:assign obj field arg)
     (define data (mk-var! "data"))
     (j:let data (translate arg)
            (j:seq
             (j:assign (translate obj) (j:set (j:deref (translate obj))
                                               (translate (mk-field field)) (translate arg))) data))]
    [(s:invoke obj meth args)
     (j:apply (j:get
               (j:deref (j:get (j:deref (translate obj)) (translate (mk-field meth)) ))
                     (mk-field (s:variable (string->symbol "$code"))))
              (cons (translate obj) (map translate args)))
     ]
    [(s:function params body) (mk-object (cons "$code" (j:lambda
                                                        (cons (j:variable (string->symbol "this"))
                                                              (if (null? params) (list) (map translate params)))
                                                        (translate body)))
                                         (cons "prototype" (mk-object)))]
    [(s:new constr args)
     (define ctor (mk-var! "ctor"))
     (define obj (mk-var! "obj"))
     (j:let ctor (j:deref (translate constr))
       (j:let obj (mk-object (cons "$proto" (j:get ctor (translate (mk-field (s:variable (string->symbol "prototype")))))))
           (j:seq (j:apply (j:get ctor (mk-field (s:variable (string->symbol "$code")))) (cons obj (map translate args))) obj
           )
       )
     )
     ]
))
