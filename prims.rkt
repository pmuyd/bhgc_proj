#lang racket


(provide prims prim?)

(define (prim? p) (set-member? prims p))

(define prims
  (list->set
   '(+ - / * < <= > >= = equal? number? modulo
       make-vector vector-ref vector-set!
       void? void not
       cons car cdr cons? null?
       error exit)))



