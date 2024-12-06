#lang racket


(require "prims.rkt")
(provide ssa-convert ssa?)


; The output of this pass should meet this specification:
(define (ssa? exp)
  (call/cc
   (lambda (escape)
     ; passes a set of in-scope variables
     (define ((recur-with vars) exp)
       (define recur (recur-with vars))
       (match exp
	 [`(let ([,(? symbol? x) ,(and (? (not/c symbol?)) (? recur))]) ,body)
	  ; no shadowing in the output:
	  (when (set-member? vars x) (escape #f))
	  ((recur-with (set-add vars x)) body)]
	 [`(if ,(? recur) ,(? recur) ,(? recur)) #t]
	 [`(call/cc ,(? recur)) #t]
	 
	 [`(,(? prim?) ,(? recur) ...) #t]
	 [`(apply ,(? prim?) ,(? recur)) #t]
	 
	 [`',(or (? boolean?) (? number?) '()) #t]
	 
	 [`(lambda (,(? symbol? x)) ,body)
	  ; no shadowing in the output:
	  (when (set-member? vars x) (escape #f))
	  ((recur-with (set-add vars x)) body)]
	 [`(,(? recur) ,(? recur)) #t]
	 [(and (? symbol? x) (? (not/c prim?)))
	  ; only references defined variables:
	  (set-member? vars x)]
	 
	 [_ #f]))
     ((recur-with (set)) exp))))

(define (alphatize exp [env (hash)])
  (define recur (lambda (e) (alphatize e env)))
  (match exp
  [`(call/cc ,e0)
   `(call/cc ,(recur e0))]
  [`(lambda (,x) ,e0)
   (define x+ (gensym x))
   `(lambda x+ ,(alphatize e0 (hash-set env x x+)))]
  [(? symbol? x) (not (prim? x))
   (hash-ref env x (lambda () (error (format "Var not found: ~a" x))))]))

; desugared? -> ssa?
(define (ssa-convert exp)
  ; unique var generator 
  (define label 0)
  (define (genvar base)
    (set! label (add1 label))
    (string->symbol (format "~a_~a" base label)))
  
  (define (convert exp env)
    (match exp
      ; mutable -> vector-ref
      [(? symbol? x)
       (if (set-member? mutable x)
           `(vector-ref ,(hash-ref env x) '0)
           (hash-ref env x x))]
      
      [(? number?) `(quote ,exp)]
      [(? boolean?) `(quote ,exp)]
      ['() ''()]
      
      [`(quote ,datum) `(quote ,datum)]

      ; lambda -> genvar
      [`(lambda (,x) ,body)
       (let ([x* (genvar x)])
         `(lambda (,x*)
            ,(convert body (hash-set env x x*))))]

      ; let -> make-vector
      [`(let ((,x ,e)) ,body)
       (let ([x* (genvar x)])
         `((lambda (,x*)
             ,(convert body (hash-set env x x*)))
           ,(if (set-member? mutable x)
                `(make-vector '1 ,(convert e env))
                (convert e env))))]

      ; set! -> vector-set!
      [`(set! ,x ,e)
       (let ([ce (convert e env)])
         `(vector-set! ,(hash-ref env x) '0 ,ce)
         )]

      [`(if ,cond ,then ,else)
       `(if ,(convert cond env)
            ,(convert then env)
            ,(convert else env))]
      
      [`(call/cc ,e)
       `(call/cc ,(convert e env))]

      ; function application
      [`(,op . ,as)
       (if (prim? op)
           `(,op ,@(map (lambda (a) (convert a env)) as))
           `(,(convert op env)
             ,@(map (lambda (a) (convert a env)) as)))]
      
      ))

  ; boxing mutable
  (define (mutable? exp)
    (let loop ([exp exp] [mutable (set)])
      (match exp
        [`(set! ,x ,_) (set-add mutable x)]
        [`(lambda (,_) ,_) mutable]
        [(? list? l) (foldl loop mutable l)]
        [_ mutable])))
  
  (define mutable (mutable? exp))

  ; run convert
  (convert exp (hash))
  )







