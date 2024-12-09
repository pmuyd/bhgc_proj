#lang racket

(require "../../emit.rkt")
(require "../../prims.rkt")

; The output of this pass should meet this specification:
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

(define (build! cpp)
  (with-output-to-file "out.cpp"
    (lambda () (display "#include \"../../prelude.h\"\n\n") (display cpp))
    #:exists 'replace)
  )

(define input
  (with-input-from-file "input"
    (lambda () (read))))

(define input-v (racket-eval input))
(build! (emit-cpp (clo-convert input)))
(system "g++ out.cpp -o bin -lgc -std=c++17 -O2")
(system "./bin > out")
(define cpp-v (with-input-from-file "out" read))
(system "rm out bin")

(with-output-to-file "output"
  (lambda ()
    (if (and (equal? input-v cpp-v))
	(print 1)
	(print 0)))
  #:exists 'replace)

