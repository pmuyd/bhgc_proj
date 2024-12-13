#lang racket


(require "prims.rkt")
(provide clo-convert emit-cpp build!)

; cps? -> clo?
(define (clo-convert exp)
  (define (T exp)
    (match exp
      [`(if ,(? symbol? x) ,b0 ,b1)
       (match-define `(,b0e ,b0free ,b0procs) (T b0))
       (match-define `(,b1e ,b1free ,b1procs) (T b1))
       `((if ,x ,b0e ,b1e)
         ,(set-union b0free b1free (set x))
         ,(set-union b0procs b1procs))]
      
      [`(let ([,(? symbol? x)
	       (,(? prim? op) ,(? symbol? xs) ...)])
	  ,body)
       (match-define `(,be ,bfree ,bprocs) (T body))
       `((let ([,x (,op ,@xs)])
	  ,be)
	 ,(set-union (set-remove bfree x) (list->set xs))
	 ,bprocs)]
      
      [`(let ([,(? symbol? x)
	       (apply ,(? prim? op) ,(? symbol? y))])
	  ,body)
       (match-define `(,be ,bfree ,bprocs) (T body))
       `((let ([,x (apply ,op ,y)])
           ,be)
         ,(set-add (set-remove bfree x) y)
         ,bprocs)]
      
      [`(let ([,(? symbol? x)
	       ',(and d (or (? boolean?) (? number?) '()))])
	  ,body)
       (match-define `(,be ,bfree ,bprocs) (T body))
       `((let ([,x ',d])
	  ,be)
	 ,(set-remove bfree x)
	 ,bprocs)]
      
      [`(let ([,(? symbol? x)
	       (lambda (,(? symbol? y) ,(? symbol? z)) ,b0)])
	  ,b1)
       (match-define `(,b0e ,b0free ,b0procs) (T b0))
       (match-define `(,b1e ,b1free ,b1procs) (T b1))
       (define pr (gensym 'pr))
       (define en (gensym 'en))
       (define freelist (set->list (set-subtract b0free (set y z))))
       `((let ([,x (make-clo ,pr ,@freelist)]) ,b1e)
	 ,(set-union (set-remove b1free x) (set-subtract b0free (set y z)))
	 ,(set-add
	   (set-union b0procs b1procs)
	   `(proc (,pr ,en ,y ,z)
		  ,(foldr (lambda (i w e0)
			    `(let ([,w (env-ref ,en ,i)]) ,e0))
			  b0e
			  (map add1 (range (length freelist)))
			  freelist))))]

      [`(halt ,(? symbol? x) ,(? symbol? y))
       `((halt ,x ,y)
	 ,(set x y)
	 ,(set))]
 
      [`(,(? symbol? fun) ,(? symbol? arg) ,(? symbol? kont))
       `((,fun ,arg ,kont)
	 ,(set fun arg kont)
	 ,(set))]))
  
  (match-define `(,maine ,free ,procs) (T exp))
  (set-add procs `(proc (main) ,maine)))


; clo? -> string? (for a cpp file)

(define (emit-cpp procs)
  (define (x->cpp x)
    (apply string-append
	   (cons "_"
		 (map (lambda (c)
			(if (regexp-match #rx"[A-Za-z0-9]" (string c))
			    (string c)
			    (format "_~a" (char->integer c))))
		      (string->list (symbol->string x))))))
  (define (e->cpp exp)
    (match exp
      [`(if ,(? symbol? x) ,b0 ,b1)
       (format " if (GETBOOL(~a))\n {\n~a }\n else\n {\n~a }\n"
	       (x->cpp x)
	       (e->cpp b0)
	       (e->cpp b1))]
      
      [`(let ([,(? symbol? x)
	       (,(? prim? op) ,(? symbol? xs) ...)])
	  ,body)
       (string-append
	(format "  any ~a = prim~a(~a);\n"
		(x->cpp x)
		(x->cpp op)
		(if (null? xs)
		    ""
		    (foldl (lambda (x acc)
			     (string-append acc ", " (x->cpp x)))
			   (x->cpp (car xs))
			   (cdr xs))))
	(e->cpp body))]

      [`(let ([,(? symbol? x)
               (env-ref ,y ,i)])
          ,body)
       (string-append
        (format " any ~a = ((any*)UNTAG(~a))[~a];" (x->cpp x) (x->cpp y) i)
        (e->cpp body))]

      [`(let ([,(? symbol? x)
	       (make-clo ,pr ,xs ...)])
	  ,body)
       (define t (gensym 'cl))
       (string-append
	(string-append 
	 (format "  any* ~a = (any*)GC_MALLOC(~a*sizeof(any));\n" t (add1 (length xs)))
	 (format "  ~a[0] = (any)(&~a);\n" t pr)
	 (foldl (lambda (i x acc)
		  (string-append acc (format "  ~a[~a] = ~a;\n" t i (x->cpp x))))
		""
		(map add1 (range (length xs)))
		xs)
	 (format "  any ~a = TAG((any)(~a), CLO_TAG);\n" (x->cpp x) t))
	(e->cpp body))]
      
      [`(let ([,(? symbol? x)
               (apply ,(? prim? op) ,(? symbol? y))])
          ,body)
       (string-append
        (format "  any ~a = applyprim~a(~a);\n"
                (x->cpp x)
                (x->cpp op)
                (x->cpp y))
        (e->cpp body))]
      
      [`(let ([,(? symbol? x) ',d])
	  ,body)
       (string-append
	(format "  any ~a = ~a;\n"
		(x->cpp x)
		(match d
		  [#t "TRUE"]
		  [#f "FALSE"]
		  ['() "EMPTY"]
		  [(? integer? n) (format "INT(~a)" n)]))
	(e->cpp body))]

      [`(halt ,x ,y)
       (format "  halt(~a);\n" (x->cpp x))]
 
      [`(,(? symbol? fun) ,(? symbol? arg) ,(? symbol? kont))
       (format " ((proc)((any*)(UNTAG(~a)))[0])(~a, ~a, ~a);\n"
               (x->cpp fun)
               (x->cpp fun)
               (x->cpp arg)
               (x->cpp kont))]

      ))
  (define (proc->cpp proc)
    (match proc
      [`(proc (main) ,e0)
       (string-append
        "int main()\n"
        "{\n"
        "  GC_INIT();\n"  ; init gc
        "  GC_find_leak = 1;\n"
        (e->cpp e0)
        "  force_gc();\n"
        "return 0;\n"
        "}\n")]
      [`(proc (,pr ,env ,arg ,kont) ,e0)
       (string-append
        (format "void ~a(any ~a, any ~a, any ~a)\n"
                pr
                (x->cpp env)
                (x->cpp arg)
                (x->cpp kont))
        "{\n"
	(e->cpp e0)
	"}\n")]))
  
  (define procs-main-last
    (sort (set->list procs) > #:key (compose length second)))
  
  (string-append
   ; forward declare
   (foldl (lambda (proc acc)
	    (match proc
	      [`(proc (,pr ,_ ,_ ,_) ,_)
	       (string-append acc (format "void ~a(any,any,any);\n" pr))]
	      [_ acc]))
	  "\n"
	  procs-main-last)
   ; define in full
   (foldl (lambda (proc acc)
	    (string-append acc (proc->cpp proc) "\n\n"))
	  "\n"
	  procs-main-last)))


(define (build! cpp)
  ; spits out the cpp string to out.cpp, including the prelude, and builds it with g++
  (with-output-to-file "out.cpp"
    (lambda () (display "#include \"prelude.h\"\n\n")
      (display cpp))
    #:exists 'replace)
  (system "g++ out.cpp -o bin -lgc -g -fmax-errors=2 -DGC_PRINT_STATS"))


