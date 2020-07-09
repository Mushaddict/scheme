#lang racket
(require "hw4-util.rkt")
(define E0 (hash))
(define E1 (hash (e:variable 'x) (e:number 10)))
(hash-ref E1 (e:variable 'x))
(define E2 (hash-set E1 (e:variable 'y) (e:number 20)))