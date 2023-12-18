#lang racket

(require eopl)
(require rackunit)
(require racket/match)
(require rackunit/text-ui)
(provide (all-defined-out))

;; * Introduction
;; 
;;   The language consists of the following:
;;   
;;  - 1. Concrete syntax ::  for programs in the arithmetic
;;       language.  Specified as a  CFG.  
;;       *Given*
;;       
;;  - 2. Abstract syntax ::  for programs in the arithmetic
;;       language.  Specified using a =define-datatype=.
;;       *Given*.
;; 
;;  - 3. Parser :: converts concrete syntax to abstract
;;                 syntax.  
;;       *To be implemented by you*.
;; 
;;  - 4. Expressible values :: a definition of domain of values
;;       expressed, or returned as a result of evaluation.
;;       *Given*.
;; 
;;  - 5. Denotable values :: A definition of domain values that can be
;;       referenced by identifiers.
;; 
;;  - 6. Environment :: definition of the evaluation context used for
;;                      lexically scoped variable bindings. 
;;       *To be implemented by you*.
;; 
;;  - 7. Interpreter Error Domain :: Divide by zero error, and type
;;       errors.  
;;       *Given*.
;; 
;;  - 8. Interpreter :: A program that maps abstract syntax to
;;                      expressible values.  
;;       *To be implemented by you*

  
;; * Concrete Syntax of the Lexical Language
;; 
;;   This section describes the concrete syntax of the LEXICAL language.

;; <exp> ::= <number>
;;          | <boolean>
;;          | <symbol>
;;          | (ifte <exp> <exp> <exp>)
;;          | (assume ([<symbol> <exp>]+) <exp>)
;;          | (<binop> <exp> <exp>)
;;          | (<unarynop> <exp>)
;;  
;; <unaryop> ::= !
;;  
;; <binop> ::= + | - | * | / | < | ==


;; * Abstract Syntac of LEXICAL Language
;; 
;; ** The AST Datatype Definition (Given)
;; 
;;    In this section, you are provided with the AST for the LEXICAL
;;    Language.  This is an extension of the AST for the ARITHMETIC
;;    language that was introducted in the previous assignment (HW03).
;; 
;;    The following new variants have been added to the AST:
;; 
;;    - *unaryop* : This is similar to =binop= but it represents
;;      operators with arity = 1.  We have only one unary operator which
;;      is the negation operator.  The negation operator expects a
;;      boolean value and returns the negation of that value.
;; 
;;    - *id-ref* : This construct is used to represent a reference to a
;;      binding in the current enviroment.  During evaluation, any
;;      occurence of id-ref is replaced by the ast associated with the
;;      id, if any such binding exists.  If the binding does not exist,
;;      evaluation fails.
;; 
;;    - *assume* : =assume= is used to create a new evaluation context
;;      ([[Environment][environment]]) for the evaluation of the associated ast.

(define-datatype ast ast?
  [unaryop (op unaryop?) (rand ast?)]
  [binop (op binop?) (rand1 ast?) (rand2 ast?)]
  [ifte (c ast?) (t ast?) (e ast?)]
  [num (n number?)]
  [bool (b boolean?)]
  [id-ref (sym id?)]
  [assume (bindings (list-of bind?)) (body ast?)])

;;   =define-datatype= auto-defines the following constructors
;;   (their signatures are shown for clarity):
;;
;;    1. =unaryop= :: [unaryop? ast?] -> ast?
;;    2. =binop= :: [binop? ast? ast?] -> ast?
;;    3. =ifte=  :: [ast? ast? ast?]   -> ast?
;;    4. =num=   :: number? -> ast?
;;    5. =bool=  :: boolean? -> ast?
;;    6. =id-ref= :: id? -> ast?
;;    7. =assume= :: list-of (bind?) -> ast?
;;
;; ** Binding
;;    
;;    A binding associates an =id= (variable/symbol) with an =ast=.  A
;;    list of such bindings makes the evaluation contenxt (environment).
;;    The body of the =assume= expression is evaluated by replacing any
;;    occurence of a bound variable with the associated =ast= found in
;;    the environment if any such binding exists.
;; 
;;    The datatype =bind= provides a single variant =make-bind= that
;;    prvides a structure with fields for the =id= (variable name) and
;;    the =ast= associated with that id.
;; 
;;    =make-bind= parameters:
;;    - =b-id=  :: Symbol used to identify the binding.
;;    - =b-ast= :: the ast that is bound to the symbol in =b-id=.

(define-datatype bind bind?
  [make-bind (b-id id?) (b-ast ast?)])

(define id? symbol?)

;; *** Helper Functions for bind datatype
;; 
;; **** Get the Id of a Binding
;;      Given a binding, the function =bind-id= returns the symbol (b-id)
;;      in the binding.
;; 
;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;; **** Get the AST from a Binding
;;      Given a binding, the function =bind-ast= returns the =ast= in the
;;      binding.

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))


;; * Semantic Domain
;;   
;; ** Expressible Values
;; 
;;    Types of values returned by evaluating an ast.
;; 
;; <expressible-value> ::= <number> | <boolean>
;; 
;;; expressible-value? : any/c -> boolean?
(define expressible-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))

;; ** Denotable Values
;; 
;;    Types of values denoted by identifiers.
;;
;; <denotable-value> ::= <number> | <boolean>

;;; denotable-value? :any/c -> boolean?
(define denotable-value?
  (lambda (thing)
    (or (number? thing)
      (boolean? thing))))


;; * Parser
;;   A parser parses an expression in concrete syntax according
;;   to a given grammar.
;; 
;; 
;; It uses three helper functions
;; 1. =op->ast=, which takes an operator symbol and returns the corresponding
;;    AST constructor
;; 2. =is-binop?=, which takes a symbol and returns a boolean value indicating
;;    whether or not the symbol is a binary operator.
;; 3. =is-unaryop?=, which takes a symbol and returns a boolean value indicating
;;    whether or not the symbol is a unary operator.
;; 
;; Now we are in a position to describe =parse=. Given an expression, parse
;; produces the corresponding AST.
;; 
;; If none of the above work, it throws an error.

(define (op->ast b)
  (match b
    ['+ 'add]
    ['- 'sub]
    ['* 'mul]
    ['/ 'div]
    ['< 'ltj]
    ['== 'eq]
    [_ 'error]
    ))

(define (is-binop? b)
  (match b
    ['+ #t]
    ['- #t]
    ['* #t]
    ['/ #t]
    ['< #t]
    ['== #t]
    [_ #f]
    ))

(define (is-unaryop? b)
  (match b
    ['! #t]
    [_ #f]
    ))

(struct exn:exec-not-implemented exn:fail ())
(define raise-exec-not-implemented
  (lambda ()
    (raise (exn:parse-error "not implemented" (current-continuation-marks)))))

;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
(define (parse exp)
  (cond
    ;;  <exp> := <number>
    [(number? exp) (num exp)]
    ;;  <exp> := <boolean>
    [(boolean? exp) (bool exp)]
    [(and (list? exp) (= (length exp) 3)) 
     (match (car exp)
    ;;  <exp> := (<binop> <exp> <exp>)
       ['+ (binop 'add (parse (cadr exp)) (parse (caddr exp)))]
       ['- (binop 'sub (parse (cadr exp)) (parse (caddr exp)))]
       ['* (binop 'mul (parse (cadr exp)) (parse (caddr exp)))]
       ['/ (binop 'div (parse (cadr exp)) (parse (caddr exp)))]
       ['< (binop 'lt? (parse (cadr exp)) (parse (caddr exp)))]
       ['== (binop 'eq? (parse (cadr exp)) (parse (caddr exp)))]
    ;; <exp> := (assume ([<symbol> <exp>]+) <exp>)
       ['assume (assume (map (lambda (x) (make-bind (car x) (parse (cadr x)))) (cadr exp)) (parse (caddr exp)))]
       [else (raise-parse-error "Not implemented")]
       )
    ]
    ;;  <exp> := (if <exp> <exp> <exp>)  ; (if <test> <then> <else>)
    [(and (list? exp) (= (length exp) 4))
     (match (car exp)
       ['if (ifte (parse (cadr exp)) (parse (caddr exp)) (parse (cadddr exp)))]
       [else (raise-parse-error "Not implemented")])
    ]
    ;; <exp> := <symbol>
    [(symbol? exp) (id-ref exp)]
    ;; <exp> := (<unaryop> <exp>)
    [(and (list? exp) (= (length exp) 2))
     (match (car exp)
       ['! (unaryop 'neg (parse (cadr exp)))]
       [else (raise-parse-error "Not implemented")])
    ]
    [else (raise-parse-error "Not implemented")]))

;; ** Parse Error (Given)
;;    If the expression is not grammatically legal, the parser raises an
;;    exception.  Exceptions in Racket are structures.  Specialised
;;    exceptions like =exn:parse-error= are built by inheriting from the
;;    base exception =exn:fail=.
;;    
(struct exn:parse-error exn:fail ())

;; The function =raise-parse-error=, given below raises an
;; =exn:parse-error= exception when invoked.
(define raise-parse-error 
 (lambda (err-msg)
   (raise (exn:parse-error err-msg (current-continuation-marks)))))


;; ** Test for Parsing
;;; Tests for parsing
(define ts-parsing
  (test-suite "parsing"
              (test-case "num" (check-equal? (parse 10) (num 10)))
              (test-case "symbol" (check-equal? (parse 'a) (id-ref 'a)))
              (test-case "add" (check-equal? (parse '(+ 10 20)) (binop 'add (num 10) (num 20))))
              (test-case "sub" (check-equal? (parse '(- 10 20)) (binop 'sub (num 10) (num 20))))
              (test-case "mul" (check-equal? (parse '(* 10 20)) (binop 'mul (num 10) (num 20))))
              (test-case "div" (check-equal? (parse '(/ 10 20)) (binop 'div (num 10) (num 20))))
              (test-case "neg true" (check-equal? (parse '(! #t)) (unaryop 'neg (bool #t))))
              (test-case "neg false" (check-equal? (parse '(! #f)) (unaryop 'neg (bool #f))))
              (test-case "bool-t" (check-equal? (parse #t) (bool #t)))
              (test-case "bool-f" (check-equal? (parse #f) (bool #f)))
              (test-case "if" (check-equal? (parse '(if #t 10 20)) (ifte (bool #t) (num 10) (num 20))))
              (test-case "failure"
                (check-exn exn:parse-error?
                           (lambda () (parse '(** 10 20)))))
              (test-case "recur" (check-equal?
                                  (parse '(+ (- 10 20) (* 20 30)))
                                  (binop 'add
                                             (binop 'sub (num 10) (num 20))
                                             (binop 'mul (num 20) (num 30)))))))
(define test-parse-assume
  (test-suite "Parsing assume construct"

    (test-case "valid : single binding"
      (check-equal? 
       (assume
        (list (make-bind 'x (num 30)))
        (binop 'add (num 100) (id-ref 'x)))
       (parse '(assume ([x 30])(+ 100 x)))))

    (test-case "valid : operation in binding"
      (check-equal? 
       (assume
        (list (make-bind 'x (binop 'add (num 2) (num 4))))
        (binop 'add (num 100) (id-ref 'x)))
       (parse '(assume ([x (+ 2 4)])(+ 100 x)))))

    (test-case "valid : multiple bindings"
      (check-equal?
       (assume
        (list (make-bind 'a (num 40)) (make-bind 'x (num 30)))
        (binop 'add (id-ref 'a) (id-ref 'x)))
       (parse '(assume ([a 40][x 30])(+ a x)))))

    (test-case "fail: no body"
      (check-exn exn:parse-error?
        (lambda()
          (parse '(assume ([a 40][x 30]))))))))


;; * Environment
;; 
;;   The LEXICAL language has variables which can be bound to values
;;   using the assume construct.  Evaluating expressions in such a
;;   language requires an evaluation context that keeps track of the
;;   variable bindings.  This evaluation context is known as an
;;   environment.
;; 
;;   An env is a union type of either:
;; 
;;   *empty-env* : An environment that does not have any
;;   variable bindings.
;; 
;;   OR
;; 
;;   *extended-env* : An extended environment consisting of a list of
;;   symbols, a list of denotable values and an outer environment.
(define-datatype env env?
  [empty-env]
  [extended-env
    (syms (list-of symbol?))
    (vals (list-of denotable-value?))
    (outer-env env?)])

;; ** Check if Environment is Empty
;; 
;;; empty-env? : env? -> boolean?
(define empty-env? (lambda (e)
                     (cases env e
                       (empty-env () #t)
                       (_ #f))))

;; ** Check if Environment contains Bindings
;; 
;;; extended-env? : env? -> boolean?
(define extended-env? (lambda (e)
                        (cases env e
                          (extended-env (syms vals outer-env) #t)
                          (_ #f))))
   
;; ** Lookup
;; 
;;; lookup-env: [env?  symbol?] -> any/c || exn:lookup-err?
(define lookup-env (lambda (e symbol) 
                      (cases env e
                        (empty-env () (raise-lookup-error))
                        (extended-env (syms vals outer-env)
                                      (if (member symbol syms)
                                          (list-ref vals (index-of syms symbol))
                                          (lookup-env outer-env symbol))))))

;; *** Lookup Error
;; 
;;     Lookup-error is raised when we try to look up the value of a
;;     symbol that is not present in our environment.
;; 
(struct exn:lookup-error exn:fail ())
(define raise-lookup-error 
  (lambda ()
    (raise (exn:lookup-error "unbound identifier" (current-continuation-marks)))))

;; *** Testing Lookup
;; 
(define test-lookup-env
  (test-suite "Lookup Env"
    (test-case "Binding is present : Lookup returns the ast"
      (let 
        ((mock-env (extended-env (list 'x) (list 10) (empty-env))))
        (check-equal? 10
                      (lookup-env mock-env 'x))))

    (test-case "Binding not found : Lookup throws error"
      (let 
        ((mock-env (extended-env (list 'x) (list 10) (empty-env))))
        (check-exn exn:lookup-error?
                   (lambda ()
                     (lookup-env mock-env 'y)))))
    (test-case "Binding found in outer env"
      (let*
        ((other-env (extended-env (list 'y) (list 10) (empty-env)))
         (mock-env (extended-env (list 'x) (list 3) other-env)))
        (check-equal? 10
                     (lookup-env mock-env 'y))))))

;; * Error domain
;;   The interpreter receives an AST, and produces a number, boolean,
;;   or throws an error.  We first define the types of errors it can
;;   throw.
;; 
;; ** Errors raised by the interpreter
;;    Like before, errors are specialised exceptions.  We are
;;    concerned with two kinds of exceptions raised during evaluation,
;;    or execution time: divide-by-zero and type-error.
;;    
;;    =exec-divide-by-zero= is raised when the numerator of a division is
;;    zero.  =exec-type-error= is raised when there is an argument type
;;    mismatch, e.g., a non-boolean value to the test of a
;;    conditional, or a boolean argument to addition, etc.

(struct exn:exec-div-by-zero exn:fail ())
(define raise-exec-div-by-zero
  (lambda ()
    (raise (exn:exec-div-by-zero "div-by-0!" (current-continuation-marks)))))

(struct exn:exec-type-mismatch exn:fail ())
(define raise-exec-type-mismatch
  (lambda ()
    (raise (exn:exec-type-mismatch "type mismatch!" (current-continuation-marks)))))
   
;;    These errors are raised as follows:
;;    - =(raise-exec-div-by-zero)=
;;    - =(raise-exec-type-mismatch)=
;;    - =(raise-lookup-error)=
;;      - This error is raised when the environment lookup operation
;;        fails.  Find the defintion [[Lookup%20Error][here]].
;; 
;;    Your interpreter will be expected to raise the above exceptions
;;    under the appropriate conditions.
;; 
;; ** Runtime checks for types
;;    The functions =typecheck-num=, =typecheck-bool= and
;;    =check-non-zero= defined below check whether a value has the
;;    right type and raise the appropriate runtime evaluator
;;    exceptions.
;; 
;;; runtime-check :: [expressible? -> boolean?], exn? -> [expressible? -> expressible? || exn?] 
(define runtime-check
  (lambda (pred? exn)
    (lambda (v)
      (if (pred? v)
          v
          (exn)))))

(define typecheck-num
  (runtime-check number?  raise-exec-type-mismatch))

(define typecheck-bool 
  (runtime-check boolean? raise-exec-type-mismatch))

(define check-non-zero
  (runtime-check (not/c zero?) raise-exec-div-by-zero))


;; * Interpreter
;; 
;; ** Mapping operators to operations
;; 
;; This function below maps the operators to their interpretation,
;; i.e., actual functions that operate on expressible values.
(define op-interpretation
  (lambda (op)
    (match op
      ['add +]
      ['sub -]
      ['mul *]
      ['div /]
      ['lt? <]
      ['eq? =]
      ['neg not]
      [_ error 'op-interpretation "unknown op"])))

;; ** =eval-ast=
;; 
;;    =eval-ast= uses =cases= from =eopl= to pattern match the abstract datatypes.
;;
(define eval-ast
  (lambda (a env)
    (cases ast a
      ;;    1. =num=   :: number? -> ast?
      (num (n) (typecheck-num n))
      ;;    2. =bool=  :: boolean? -> ast?
      (bool (b) (typecheck-bool b))
      ;;    3. =id-ref= :: id? -> ast?
      (id-ref (sym) (lookup-env env sym))
      ;;    4. =ifte=  :: [ast? ast? ast?]   -> ast?
      (ifte (c t e)
            (let ([rc (typecheck-bool (eval-ast c env))])
              (if rc (eval-ast t env) (eval-ast e env))
              ))
      ;;    5. =assume= :: list-of (bind?) -> ast?
      (assume (bindings body)
              (let ([syms (map bind-id bindings)]
                    [vals (map (lambda (b) (eval-ast (bind-ast b) env)) bindings)])
                (eval-ast body (extended-env syms vals env))))
      ;;    6. =binop= :: [binop? ast? ast?] -> ast?
      (binop (op rand1 rand2)
             (let ([r1 (typecheck-num (eval-ast rand1 env))]
                   [r2 (typecheck-num (eval-ast rand2 env))])
               (if (eq? op 'div) ((op-interpretation op) r1 (check-non-zero r2))  ((op-interpretation op) r1 r2))
               ))
      ;;    7. =unaryop= :: [unaryop? ast?] -> ast?
      (unaryop (op rand)
               (let ([r (typecheck-bool (eval-ast rand env))])
                 ((op-interpretation op) r)))
      [_ (raise-exec-not-implemented)]
      )))

;; ** Helper Functions for Evaluation
;;   
;; *** unaryop?
;; 
;;    =(unaryop x)= checks if the given symbol is a valid unary operator.
(define unaryop?
  (lambda (x)
    (match x
      ['neg #t]
      [_ #f])))

;; *** binop?
;; 
;;    =(binop x)= checks if the given symbol is a valid binary operator.
(define binop?
  (lambda (x)
    (match x
      ['add #t]
      ['sub #t]
      ['mul #t]
      ['div #t]
      ['lt? #t]
      ['eq? #t]
      [_ #f])))

;; ** Testing =eval-ast=
;;    
;; *** Routine test cases
(define ts-evaluation
  (test-suite
    "evaluation"
    (test-case "num" 
               (check-equal? 10 
                             (eval-ast (num 10) (empty-env))))
    (test-case "id-ref" 
               (check-equal? 3
                             (eval-ast (id-ref 'x) 
                             (extended-env '(x v) (list 3 5) (empty-env)))))
    (test-case "add" 
               (check-equal? 30 
                             (eval-ast (binop 'add (num 10) (num 20)) (empty-env))))
    (test-case "sub" 
               (check-equal? -10 
                             (eval-ast (binop 'sub (num 10) (num 20)) (empty-env))))
    (test-case "mul" 
               (check-equal? 200 
                             (eval-ast (binop 'mul (num 10) (num 20)) (empty-env))))
    (test-case "lt" 
               (check-equal? #t 
                             (eval-ast (binop 'lt? (num 10) (num 20)) (empty-env))))
    (test-case "eq" 
               (check-equal? #t 
                             (eval-ast (binop 'eq? (num 10) (num 10)) (empty-env))))
    (test-case "div-success" 
               (check-equal? 2
                             (eval-ast (binop 'div (num 20) (num 10)) (empty-env))))
    (test-case "div-failure"
               (check-exn exn:exec-div-by-zero?
                          (lambda () 
                            (eval-ast (binop 'div (num 20) (num 0)) (empty-env)) 2)))
    (test-case "bool-t" 
               (check-equal? #t
                             (eval-ast (bool #t) (empty-env))))
    (test-case "bool-f" 
               (check-equal? #f 
                             (eval-ast (bool #f) (empty-env))))
    (test-case "if-true" 
               (check-equal? 10 
                             (eval-ast 
                               (ifte (bool #t) (num 10) (num 20)) (empty-env))))
    (test-case "if-false" 
               (check-equal? 20
                             (eval-ast 
                               (ifte (bool #f) (num 10) (num 20)) (empty-env))))
    (test-case "if-type-mismatch"  
               (check-exn exn:exec-type-mismatch?
                          (lambda () 
                            (eval-ast 
                              (ifte (num 42) (num 10) (num 20)) (empty-env)))))
    (test-case "assume : single binding : empty-env at top level : no reference"
               (check-equal? 50
                             (eval-ast 
                               (assume (list (make-bind 'x (num 100)))
                                       (binop 'add (num 2) (num 48)))
                               (empty-env))))
    (test-case "assume : single binding : empty-env at top level : single reference"
               (check-equal? 150
                             (eval-ast 
                               (assume (list (make-bind 'x (num 100)))
                                       (binop 'add (num 50) (id-ref 'x)))
                               (empty-env))))))

;; *** Testing Incorrect rand1 type
(define ts-numop-incorrect-param-rand1
  (test-suite 
   "wrongly typed rand1 parameters"
   (for/list ([numerical-op '(add sub mul div lt? eq?)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op
                                     (binop 'lt? (num 10) (num 20)) ; boolean
                                     (num 10))
                              (empty-env))))))))

;; *** Testing Incorrect rand2 type
(define ts-numop-incorrect-param-rand2
(test-suite
 "wrongly typed rand2 parameters"
 (for/list ([numerical-op '(add sub mul div)])
   (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
     (check-exn exn:exec-type-mismatch?
                (lambda () 
                  (eval-ast (binop numerical-op (num 10)
                                   (binop 'lt? (num 10) (num 20)))
                            (empty-env))))))))



;; * Test Runners
;; 
;;   These run the tests that have been written in this file.  When
;;   submitting, please ensure that all these tests pass.
(define run-all-tests 
  (lambda ()
    (run-tests ts-parsing)
    (run-tests test-parse-assume)
    (run-tests test-lookup-env)
    (run-tests ts-evaluation)
    (run-tests ts-numop-incorrect-param-rand1)
    (run-tests ts-numop-incorrect-param-rand2)))

;; * Running the test suite
;; 
;;   We will use [[https://docs.racket-lang.org/raco/index.html][raco]] command line utility to run the tests.
;; 
;;   =raco test test.rkt= will run the test suite.
  (module+ test
    (run-all-tests))
