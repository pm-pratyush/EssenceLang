#lang racket

(require eopl)
(require rackunit)
(require racket/match)
(require rackunit/text-ui)

(provide (all-defined-out))

;; * Assignment on Recursive Language
;;
;; In this assignment, you are required to implement a parser and
;; interpreter for a recursive language.
;;
;; The recursive language extends the existing language with the
;; =function= and =recursive= construct.
;;
;; The language consists of the following:
;;
;; - 1. Concrete syntax ::  for programs in the arithmetic
;;       language.  Specified as a  CFG.  
;;       *Given*
;;       
;; - 2. Abstract syntax ::  for programs in the arithmetic
;;       language.  Specified using a =define-datatype=.
;;       *Given*.
;; 
;; - 3. Parser :: converts concrete syntax to abstract
;;                 syntax.  
;;       *To be implemented by you*.
;; 
;; - 4. Expressible values :: a definition of domain of values
;;       expressed, or returned as a result of evaluation.
;;       *Given*.
;; 
;; - 5. Denotable values :: A definition of domain values that can be
;;       referenced by identifiers.
;; 
;; - 6. Environment :: definition of the evaluation context. 
;;       *Given (You need to implement the lookup function)*
;; 
;; - 8. Interpreter :: A program that maps abstract syntax to
;;                      expressible values.  
;;       *To be implemented by you*

;; -----

;; Two new constructs have been added that were not present in the
;; lexical language: =function= and =app= for functional application.
;; 
;; ** Function
;; 
;; The =function= construct allows you to write a function.  This
;; function can be applied to the given arguments using =app=.
;; 
;; A function to add two numbers can be defined as follows:
;;    
;; (function (x y) (+ x y))
;; 
;; This function can be applied to given numbers:
;; 
;; ((function (x y) (+ x y)) 4 5)
;; 
;; A function can be bound to an identifier:
;; 
;; (assume ([sub (function (a b) (- a b))])(sub 10 3))
;;
;; * Why we have a seperate =recursive= construct.
;; 
;; ** =function= cannot handle recursive functions
;; The =function= construct does not allow you to write a recursive
;; function.
;; 
;; Let's say you want to define the factorial function.
;; 
;; (assume ([factorial (function (n)
;;                       (ifte (eq? 0 n) 
;;                       1 
;;                       (* n (factorial (- n 1)))))])
;;   (factorial 4))
;; 
;; This would parse, but you may get an error during evaluation because
;; the environment in the closure for the factorial function does not
;; contain the binding for =factorial= that you just defined.
;; 
;; Note that you may not get an error if the environment has some other
;; function bound to =factorial= defined earlier, but that is not the
;; intended behavior.
;;
;; * A new construct for recursive bindings
;; To ovecome the limitations discussed above, we introduce a new
;; construct called =recursive=.  The =recursive= construct allows you to
;; use any binding from the current set of bindings in the bound
;; expressions.
;;
;; The example (factorial) seen above can be written using the
;; =recursive= construct as follows:
;; 
;; (recursive ([factorial (n) 
;;                (ifte (eq? 0 n) 
;;                      1 
;;                      (* n (factorial (- n 1))))])
;;   (factorial 4))


;; * Concrete Syntax
;;
;; The complete grammar for the recursive language is as follows:
;; 
;; <exp> ::= <number>
;;         | <boolean>
;;         | <symbol>
;;         | (ifte <exp> <exp> <exp>)
;;         | (assume ([<symbol> <exp>]*) <exp>)
;;         | (function (<symbol>*) <exp>)
;;         | (recursive ([<symbol> (<symbol>*) <exp>]*) <exp>)
;;         | (<exp> <exp>*)

;; * Abstract Syntax
;;
;; The complete AST for the Recursive Lanauge is as follows:
(define *keywords*
  '(ifte function assume recursive))

(define id?
  (lambda (x)
    (and
     (symbol? x)
     (not (memq x *keywords*)))))

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [ifte (test ast?) (then ast?) (else-ast ast?)]
  [function (formals (list-of id?)) (body ast?)]
  [recursive (fbinds (list-of fbind?)) (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  [id-ref (sym id?)]
  [assume (binds  (list-of bind?)) (body ast?)])

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

(define get-bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

(define get-bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

(define get-body
  (lambda (c)
    (cases proc c
     [closure (formals body env) body]
     [else (error "Non-closure parameter")])))

(define get-formals
  (lambda (c)
    (cases proc c
      [closure (formals body env) formals]
      [else (error "Non-closure parameter")])))

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

;; ** =fbind= datatype
;; The =fbind= datatype is used to define a function binding in the
;; =recursive= bindings.
;; 
;; There is only one variant =make-fbind= with the following fields:
;; 
;;  =fb-id= :: function identifier.
;; - =fb-formals= :: list of parameters for the function.
;; - =fb-body= :: body of the function.

(define-datatype fbind fbind?
  [make-fbind (fb-id id?)
              (fb-formals (list-of id?))
              (fb-body ast?)])

;;; fbind-id : fbind? -> id?
(define fbind-id
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-id])))

;;; fbind-formals : fbind? -> (list-of id?)
(define fbind-formals
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-formals])))

;;; fbind-body : fbind? -> ast?
(define fbind-body
  (lambda (b)
    (cases fbind b
      [make-fbind (fb-id fb-formals fb-body) fb-body])))

;; * Parser
;; =parse= function converts the concrete syntax of the recursive lanauge into the
;; ast representation.

;;; parse :: any/c -> ast?  Raises exception exn?
;;; Fill in the function parse here
(define (parse exp)
  (cond
    ;; <exp> := <empty>
    [(empty? exp) (list)]
    
    ;;  <exp> := <number>
    [(number? exp) (num exp)]
    ;;  <exp> := <boolean>
    [(boolean? exp) (bool exp)]
    ;; <exp> :== <symbol>
    [(id? exp) (id-ref exp)]

    [(and (list? exp) (= (length exp) 3) (or (eq? (car exp) 'assume) (eq? (car exp) 'function) (eq? (car exp) 'recursive)))
     (match (car exp)
    ;; <exp> := (assume ([<symbol> <exp>]+) <exp>)
       ['assume (assume (map (lambda (x) (make-bind (car x) (parse (cadr x)))) (cadr exp)) (parse (caddr exp)))]
    ;; <exp> := (function (<symbol>*) <exp>)
       ['function (function (cadr exp) (parse (caddr exp)))]
    ;; <exp> := (recursive ([<symbol> (<symbol>*) <exp>]+) <exp>)
       ['recursive (recursive (map (lambda (x) (make-fbind (car x) (cadr x) (parse (caddr x)))) (cadr exp)) (parse (caddr exp)))]
       )
    ]
    ;;  <exp> := (if <exp> <exp> <exp>)  ; (if <test> <then> <else>)
    [(and (list? exp) (= (length exp) 4) (eq? (car exp) 'ifte))
     (match (car exp)
       ['ifte (ifte (parse (cadr exp)) (parse (caddr exp)) (parse (cadddr exp)))])
    ]
    ;; <exp> := (<exp> <exp>*)
    [else (app (parse (car exp)) (map parse (cdr exp)))]
))

;; ** Test

(define test-f1
  (test-case "f1"
    (check-equal? (parse '(recursive ([f1 (x) (< 5 x)])(ifte (f1 2) 0 10)))
                  (recursive (list 
                                (make-fbind 'f1
                                            '(x)
                                            (app (id-ref '<) (list (num 5) (id-ref 'x)))))
                              (ifte (app (id-ref 'f1) (list (num 2)))
                                    (num 0) 
                                    (num 10))))))

(define test-no-params
  (test-case "no params to recur func"
    (check-equal? (parse '(recursive ([v () 3]) v))
                  (recursive (list (make-fbind 'v '() (num 3))) (id-ref 'v)))))


(define test-multi-binds
  (test-case "multiple binds"
    (check-equal? (parse '(recursive ([f (x) (+ x x)] [g (y) (- (f y) 1)]) (g 1)))
                  (recursive (list (make-fbind 'f '(x) (app (id-ref '+) (list (id-ref 'x) (id-ref 'x))))
                                   (make-fbind 'g '(y) (app (id-ref '-) 
                                                            (list (app (id-ref 'f) (list (id-ref 'y)))
                                                                  (num 1)))))
                             (app (id-ref 'g) (list (num 1)))))))

(define test-recursive-parsing
  (test-suite "Recursive Parsing"
    test-f1
    test-no-params
    test-multi-binds))

;; ** Procedure
;;   
;; A procedure is either a =prim-proc= or a =closure=. A =prim-proc=
;; refers to an inbuilt scheme procedure.  A closure is used for a
;; user-defined function.
(define-datatype proc proc?
  [prim-proc
    ;; prim refers to a scheme procedure
    (prim procedure?)
    ;; sig is the signature
    (sig (list-of procedure?))] 
  [closure
    (formals (list-of symbol?))
    (body ast?)
    (env env?)])

;;; prim? : proc? -> boolean?
(define prim-proc?
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #t]
      [else #f])))

(define closure? 
  (lambda (p)
    (cases proc p
      [prim-proc (prim sig) #f]
      [else #t])))

;; * Semantic Domain
;; 
;; The expressible and denotable values now include procedures along
;; with numbers and booleans.  A =Procedure= is the ast representation
;; of a function.
;;
;; *** Signature (Sig)
;; The signature of a =prim-proc= defines the type of its return
;; value and the type of each of its parameters.
;; 
;; It is a list of predicates in which the first element denotes the
;; return type and the rest of the list denotes the types of each of
;; the arguments.
;; 
;; For example, the signature of =<= (less than) would be =(list
;; boolean? number? number?)=.
;; 
;; *** Closure
;; 
;; A closure provides the execution context (environment) required to
;; evaluate the function.  A closure consists of the three things:
;; formals, body and env.
;; 
;; - Formals is the list of symbols that denote the formal parameters of
;; the function.
;; 
;; - Body is the expression that is evaluated to given the result of
;; function evaluation.
;; 
;; - Env is the environment (context) in which the boby is evaluated.
;;    
;; During the evalution (application) of a function, the environment
;; contains bindings for all the formal parameters.

;; ** Expressible Values
;; 
;; Types of values returned by evaluating an ast.
;; 
;; <expressible-value> ::= <number> | <boolean> | <proc>

;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (or/c number? boolean? proc?))

;; ** Denotable Values
;; 
;; Types of values denoted by identifiers.
;; 
;; <denotable-value> ::= <number> | <boolean> | <proc>

;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (or/c number? boolean? proc?))

;; * Recursive Environment (YOU NEED TO IMPLEMENT THIS)
;; 
;; Evaluating expressions requires an evaluation context that keeps
;; track of the variable bindings.  This evaluation context is known as
;; an environment.
;; 
;; An env is a union type of either:
;; 
;; *empty-env* : An environment that does not have any
;; variable bindings.
;; 
;; OR
;; 
;; *extended-env* : An extended environment consisting of a list of
;; symbols, a list of denotable values and an outer environment.
;; 
;; OR
;; 
;; *extended-rec-env* : The extended-rec-env variant is used to define
;; the recursive function bindings created using the =recursive=
;; construct.

(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of denotable-value?))
    (outer-env env?)]
  [extended-rec-env
    (fsyms (list-of symbol?))
    (lformals (list-of (list-of symbol?)))
    (bodies (list-of ast?))
    (outer-env env?)])

;; ** Predicates

;;; empty-env? : env? -> boolean?
(define empty-env?
  (lambda (e)
    (cases env e
      [empty-env () #t]
      [else #f])))

;;; extended-env? : env? -> boolean?
(define extended-env?
  (lambda (e)
    (cases env e
      [extended-env (syms vals outer-env) #t]
      [else #f])))

;;; extended-rec-env? : env? -> boolean?
(define extended-rec-env?
  (lambda (e)
    (cases env e
      [extended-rec-env (fsyms lformals bodies outer-env) #t]
      [else #f])))

;; *** Test

(define e1
  (extended-env '(x y z) '(1 2 3) (empty-env)))

(define e2
  (extended-env '(w x) '(5 6) e1))

(define even-body
  (ifte
    (app (id-ref '0?) (list (id-ref 'n)))
    (bool #t)
    (app
      (id-ref 'odd?)
      (list (app
              (id-ref '-)
              (list (id-ref 'n) (num 1)))))))

(define odd-body
  (ifte (app (id-ref '0?) (list (id-ref 'n)))
    (bool #f)
    (app (id-ref 'even?)
      (list (app (id-ref '-) (list (id-ref 'n) (num 1)))))))

(define e3
  (extended-rec-env
    '(even? odd?)
    '((n) (n))
    (list even-body odd-body)
    e2))

;; ** Lookup
;; The function =(lookup-env e x)= is used to get the value of the
;; binding =x= in the environment =e=. (TO BE IMPLEMENTED BY YOU)
(define (find-index element lst)
  (define (find-index-helper lst index)
    (cond
      [(null? lst) #f]                 ; Element not found
      [(equal? (car lst) element) index] ; Element found, return the index
      [else (find-index-helper (cdr lst) (+ index 1))])) ; Recurse with the rest of the list and an updated index
  (find-index-helper lst 0))

(define lookup-env
  (lambda (e x) 
    (cases env e
      [empty-env () (error "No binding for x in e")]
      [extended-env (syms vals outer) (cond
                                        [(find-index x syms) (list-ref vals (find-index x syms))]
                                        [else (lookup-env outer x)])]
      [extended-rec-env (fsyms lformals bodies outer) (cond
                                                        [(find-index x fsyms) (closure (list-ref lformals (find-index x fsyms)) (list-ref bodies (find-index x fsyms)) e)]
                                                        [else (lookup-env outer x)])]
      )))

(define test-env
  (test-case "outer env"
    (check-equal? 6 (lookup-env e3 'x))))

(define test-rec-env
  (test-case "Outer Rec Env"
    (check-equal?
      (closure '(n) even-body e3)
      (lookup-env e3 'even?))))

(define lookup-test
  (test-suite "Lookup"
    test-env
    test-rec-env))

;; * Operators as Procedures (YOU NEED TO IMPLEMENT THIS)
;; 
;; In the lexical language we defined operators as separate constructs.
;; In this language we will use the new =prim-proc= construct to
;; implement operators.
;; 
;; The initial environment will contain bindings for all the operator
;; symbols.  You need to implement the corresponding procedures for
;; each operator.
;; 
;; Operators: 
;; - + :: Add two numbers
;; - - :: Sub two numbers
;; - * :: Product of two numbers
;; - / :: Divide first number by the second non-zero number.
;; - < :: (< a b) -> a is less than b?
;; - <= :: less than or equal?
;; - eq? :: two numbers (or booleans) are equal?
;; - 0? :: is the given number equal to zero?
;; - ! :: negation (#t -> #f, #f -> #t)
(define +p (prim-proc (lambda (x y) (+ x y)) (list number? number? number?)))
;; Implement these:
(define -p (prim-proc (lambda (x y) (- x y)) (list number? number? number?)))
(define *p (prim-proc (lambda (x y) (* x y)) (list number? number? number?)))
(define /p (prim-proc (lambda (x y) (/ x y)) (list number? number? number?)))
(define <p (prim-proc (lambda (x y) (< x y)) (list boolean? number? number?)))
(define <=p (prim-proc (lambda (x y) (<= x y)) (list boolean? number? number?)))
(define eq?p (prim-proc (lambda (x y) (eq? x y)) (list boolean? number? number?)))
(define 0?p (prim-proc (lambda (x) (eq? x 0)) (list boolean? number?)))
(define !p (prim-proc (lambda (x) (not x)) (list boolean? boolean?)))

;; * Interpreter
;; The =eval-ast= function takes an =ast= and the corresponding =env=
;; (that contains bindings for evaluation of the =ast=) and returns the
;; evaluated expressible value.
(define eval-ast
  (lambda (a e)
    (cases ast a
      [num (n) n]
      [bool (b) b]
      ;; eval-id-ref: lookup the value of the identifier in the environment
      [id-ref (id) (lookup-env e id)]
      ;; eval-ifte: evaluate the test expression.  If the result is true,
      [ifte (test then-clause else-clause) (if (eval-ast test e)
                                              (eval-ast then-clause e)
                                              (eval-ast else-clause e))]
      ;; eval-assume: extend the environment with the bindings and evaluate the body
      [assume (binds body) (eval-ast body (extended-env (map bind-id binds) (map (lambda (x) (eval-ast (bind-ast x) e)) binds) e))]
      ;; eval-function: return a closure
      [function (formals body) (closure formals body e)]
      ;; eval-recursive: extend the environment with the bindings and evaluate the body
      [app (rator rands) (let ([p (eval-ast rator e)])
                           (cases proc p
                             [prim-proc (prim sig) (apply prim (map (lambda (x) (eval-ast x e)) rands))]
                             [closure (formals body env) (eval-ast body (extended-env formals (map (lambda (x) (eval-ast x e)) rands) env))]))]
      ;; eval-recursive: extend the environment with the bindings and evaluate the body
      [recursive (fbinds body) (eval-ast body (extended-rec-env (map fbind-id fbinds) (map fbind-formals fbinds) (map fbind-body fbinds) e))]
      )
    ))

(define *init-env*
  (extended-env
   '(+ - * / < <= eq? 0? !)
   (list +p -p *p /p <p <=p eq?p 0?p !p)
   (empty-env)))

;; ** Test

(define test-even-odd
 (test-case "Even Odd"
  (check-equal?
   (eval-ast
    (recursive
     (list
      (make-fbind 'even?
                  '(n)
                  (ifte (app (id-ref '0?) (list (id-ref 'n)))
                        (bool #t)
                        (app (id-ref 'odd?)
                             (list (app (id-ref '-) (list (id-ref 'n) (num 1)))))))
      
      (make-fbind 'odd?
                  '(n)
                  (ifte (app (id-ref '0?) (list (id-ref 'n)))
                        (bool #f)
                        (app (id-ref 'even?)
                             (list (app (id-ref '-) (list (id-ref 'n) (num 1))))))))

     (app (id-ref 'even?) (list (num 3))))
     *init-env*)
   #f)))


(define test-factorial
 (test-case "factorial"
  (check-equal?
   (eval-ast (parse '(recursive ([f (n) (ifte (0? n) 1 (* n (f (- n 1))))])
         (f 3))) *init-env*)
   6)))


(define test-recursive-evaluation
  (test-suite "test-eval"
   test-even-odd
   test-factorial))

(define test-recursive
  (test-suite "Recursive Tests"
              test-recursive-parsing
              lookup-test
              test-recursive-evaluation))

(define run-all-tests
  (lambda ()
    (run-tests test-recursive)))

(module+ test
  (run-all-tests))
