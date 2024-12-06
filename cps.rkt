#lang racket


(require "prims.rkt")
(provide cps-convert anf-convert racket-eval anf? cps?)


; An eval function suitable for the input and output languages
(define (racket-eval exp)
  (eval
   `(call/cc
     (lambda (halt)
       (let ([halt (lambda (r _) (halt r))])
	 (with-handlers ([exn:fail?
			  (lambda (e) 1234567)])
			,exp))))
   (parameterize ([current-namespace (make-base-namespace)])
     (namespace-require 'racket)
     (current-namespace))))


; The output of anf-convert should meet this specification 
;     (all sub-expressions are let-bound):
(define (anf? exp)
  (define ((recur-with vars) exp)
    (define recur (recur-with vars))
    (match exp
      [`(let ([,(? symbol? x) ,(and (? (not/c symbol?)) (? recur))]) ,body)
       (and (not (set-member? vars x))
	    ((recur-with (set-add vars x)) body))]
      [`(if ,(? symbol? x) ,(? recur) ,(? recur))
       (set-member? vars x)]
      [`(call/cc ,(? symbol? x))
       (set-member? vars x)]
      
      [`(,(? prim?) ,(? symbol? xs) ...)
       (andmap (lambda (x) (set-member? vars x)) xs)]
      [`(apply ,(? prim?) ,(? symbol? x))
       (set-member? vars x)]
      
      [`',(or (? boolean?) (? number?) '()) #t]
      
      [`(lambda (,(? symbol? x)) ,body)
       (and (not (set-member? vars x))
	    ((recur-with (set-add vars x)) body))]
      [`(,(? symbol? f) ,(? symbol? a))
       (and (set-member? vars f) (set-member? vars a))]
      [(and (? symbol? x) (? (not/c prim?)))
       (set-member? vars x)]
      
      [_ #f]))
  ((recur-with (set)) exp))


; The output of cps-convert should meet this specification (continuation-passing style)
(define (cps? exp)
  (define ((recur-with vars) exp)
    (define recur (recur-with vars))
    (match exp
      [`(if ,(? symbol? x) ,(? recur) ,(? recur))
       (set-member? vars x)]
      
      [`(let ([,(? symbol? x)
	       (,(? prim?) ,(? symbol? xs) ...)])
	  ,body)
       (and (not (set-member? vars x))
	    (andmap (lambda (x) (set-member? vars x)) xs)
	    ((recur-with (set-add vars x)) body))]
      
      [`(let ([,(? symbol? x)
	       (apply ,(? prim?) ,(? symbol? y))])
	  ,body)
       (and (not (set-member? vars x))
	    (set-member? vars y)
	    ((recur-with (set-add vars x)) body))]
      
      [`(let ([,(? symbol? x)
	       ',(or (? boolean?) (? number?) '())])
	  ,body)
       (and (not (set-member? vars x))
	    ((recur-with (set-add vars x)) body))]
      
      [`(let ([,(? symbol? x)
	       (call/cc ,(? symbol? y))])
	  ,body)
       (and (not (set-member? vars x))
	    (set-member? vars y)
	    ((recur-with (set-add vars x)) body))]
      
      [`(let ([,(? symbol? x)
	       (lambda (,(? symbol? y) ,(? symbol? z)) ,lambody)])
	  ,letbody)
       (and (not (set-member? vars x))
	    (not (set-member? vars y))
	    (not (set-member? vars z))
	    ((recur-with (set-union vars (set y z))) lambody)
	    ((recur-with (set-add vars x)) letbody))]
 
      [`(,(? symbol? fun) ,(? symbol? arg) ,(? symbol? kont))
       (andmap (lambda (x) (set-member? vars x)) exp)]
      
      [_ #f]))
  ((recur-with (set 'halt)) exp))


; ssa? -> anf?
(define (anf-convert exp)
  (define (normalize exp k)
    (match exp
      [`(if ,ge ,te ,ee)
       (normalize-name ge
                       (lambda (t)
                         (k `(if ,t
                                 ,(normalize-term te)
                                 ,(normalize-term ee)))))]
      
      [`(let ([,x ,rhs]) ,body)
       (normalize rhs
                  (lambda (rhs-anf)
                    `(let ([,x ,rhs-anf])
                       ,(normalize body k))))]
      
      [`(lambda (,p) ,body)
       (k `(lambda (,p) ,(normalize-term body)))]
      
      [`(call/cc ,e0)
       (normalize-name e0
                       (lambda (t)
                         (k `(call/cc ,t))))]
      
      [`(apply ,(? prim? op)  ,e0)
       (normalize-name e0
                       (lambda (t)
                         (k `(apply ,op ,t))))]
      
      [`(,(? prim? op) ,es ...)
       (normalize-name* es
                        (lambda (t*)
                          (k `(,op . ,t*))))]
      
      [`',d
       (k `',d)]
      
      [`(,fe ,ae)
       (normalize-name* exp
                        (lambda (t*)
                          (k t*)))]

      [(? (lambda (x) (or (number? x) (boolean? x))))
       (k exp)]
      
      [(? symbol? x)
       (k x)]))
  
  (define (normalize-term exp)
    (normalize exp (lambda (x) x)))
  
  (define (normalize-name exp k)
    (normalize exp
               (lambda (exp-anf)
                 (if (symbol? exp-anf)
                     (k exp-anf)
                     (let ([t (gensym 't)])
                       `(let ([,t ,exp-anf]) ,(k t)))))))
  
  (define (normalize-name* exp* k)
    (if (null? exp*)
        (k '())
        (normalize-name (car exp*)
                        (lambda (t)
                          (normalize-name* (cdr exp*)
                                           (lambda (t*)
                                             (k `(,t . ,t*))))))))
  
  (normalize-term exp))

; anf? -> cps?
(define (cps-convert exp [c 'halt])
  (define (atomic? exp)
    (match exp
      [`(lambda (,p) ,b) #t]
      [`(apply ,(? prim?) ,(? symbol?)) #t]
      [`(,(? prim?) ,(? symbol?) ...) #t]
      [`',d #t]
      [_ #f]))
  (match exp
    [`(let ([,x (lambda (,y) ,lambody)]) ,letbody)
     (define k (gensym 'k))
     `(let ([,x (lambda (,y ,k) ,(cps-convert lambody k))])
        ,(cps-convert letbody c))]

    [(? atomic? ae)
     (define t (gensym 't))
     (cps-convert `(let ([,t ,ae]) ,t) c)]
    
    [`(if ,(? symbol? gx) ,then ,else)
     `(if ,gx
          ,(cps-convert then c)
          ,(cps-convert else c))]
    
    [`(let ([,x ,(? atomic? aexp)]) ,body)
     `(let ([,x ,aexp]) ,(cps-convert body c))]

    [`(let ([,x ,rhs]) ,body)
     
     (define k (gensym 'k))
     `(let ([,k (lambda (,x ,(gensym '_))
                  ,(cps-convert body c))])
        ,(cps-convert rhs k))]

    [`(,(? symbol? f) ,(? symbol? a))
     #:when (not (prim? f))
     `(,f ,a ,c)]
    
    [(? symbol? x)
     `(,c ,x ,c)]

    ))

(define test
  '(let ([x 5])
     (+ 1 (call/cc (lambda (k)
                     (* 2 (k x)))))))

(define result (anf-convert test))
(pretty-print result)
(anf? result)
(racket-eval test)
(racket-eval result)



