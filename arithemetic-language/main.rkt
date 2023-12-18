#lang racket
(require eopl)
(require rackunit)
(require racket/match)
(require rackunit/text-ui)
(provide (all-defined-out))

;; In this assignment you will be implementing an interpreter
;; for the ARITHMETIC Language, a language of arithmetic
;; expressions. 

;; The language consists of the following:
;; 
;;  - 1. Concrete syntax ::  for programs in the arithmetic
;;       language.  Specified as a  CFG.
;;       *Given*
;; 
;;  - 2. Abstract syntax ::  for programs in the arithmetic
;;       language.  Specified using a `define-datatype`.
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
;;  - 5. Interpreter Error Domain :: Divide by zero error, and type
;;       errors.
;;       *Given*.
;; 
;;  - 6. Interpreter :: A program that maps abstract syntax to
;;                      expressible values.
;;       *To be implemented by you*

;; Concrete Syntax of ARITHMETIC
;;  <exp> := <number>
;;       |   <boolean>
;;       |   (if <exp> <exp> <exp>)  ; (if <test> <then> <else>)
;;       |   (<binop> <exp> <exp>)

;;  <binop> := + | - | * | / | < | ==

;; Abstract Syntax of ARITHMETIC

(define-datatype ast ast?
 [binop (op binop?) (rand1 ast?) (rand2 ast?)]
 [ifte (c ast?) (t ast?) (e ast?)]
 [num (n number?)]
 [bool (b boolean?)])

;; =define-datatype= auto-defines the following constructors
;;    (their signatures are shown for clarity):
;;
;;    1. =binop= :: [binop? ast? ast?] -> ast?
;;    2. =ifte=  :: [ast? ast? ast?]   -> ast?
;;    3. =num=   :: number? -> ast?
;;    4. =bool=  :: boolean? -> ast?
;;
;;  binop? :: symbol? -> boolean? (Given)
;;  (binop? op) checks if op one of the legal binary
;;  operators. 

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

;; Parser (You need to implement this)

;;  Parse Error (Given)
;;
;;  If the expression is not grammatical legal, the parser raises an
;;  exception.  Exceptions in Racket are structures.  Specialised
;;  exceptions like `exn:parse-error` are built by inheriting from
;;  the base exception `exn:fail`. 
(struct exn:parse-error exn:fail ())

;; The function =raise-parse-error=, given below raises an
;; =exn:parse-err= exception when invoked.

(define raise-parse-error 
 (lambda (err-msg)
   (raise (exn:parse-error err-msg (current-continuation-marks)))))

;; You need to complete this definition.

;; =parse= checks if the passed symbol matches any of the concrete
;; syntax.  If so, it generates the appropriate AST by calling the
;; corresponding constructor. It recursively calls on further
;; symbols for appropriate constructor arguments.
;; 
;;; parse :: any/c -> ast?  Raises exception exn:parse-error?
;;; Fill in the function parse here
(define (parse exp)
  (cond
    ;;  <exp> := <number>
    [(number? exp) (num exp)]
    ;;  <exp> := <boolean>
    [(boolean? exp) (bool exp)]
    ;;  <exp> := (<binop> <exp> <exp>)
    [(and (list? exp) (= (length exp) 3)) 
     (match (car exp)
       ['+ (binop 'add (parse (cadr exp)) (parse (caddr exp)))]
       ['- (binop 'sub (parse (cadr exp)) (parse (caddr exp)))]
       ['* (binop 'mul (parse (cadr exp)) (parse (caddr exp)))]
       ['/ (binop 'div (parse (cadr exp)) (parse (caddr exp)))]
       ['< (binop 'lt? (parse (cadr exp)) (parse (caddr exp)))]
       ['== (binop 'eq? (parse (cadr exp)) (parse (caddr exp)))]
       [else (raise-parse-error "Not implemented")]
       )
    ]
    ;;  <exp> := (if <exp> <exp> <exp>)  ; (if <test> <then> <else>)
    [(and (list? exp) (= (length exp) 4))
     (match (car exp)
       ['if (ifte (parse (cadr exp)) (parse (caddr exp)) (parse (cadddr exp)))]
       [else (raise-parse-error "Not implemented")])
    ]
    [else (raise-parse-error "Not implemented")]))

;; Test for Parsing
(define ts-parsing
  (test-suite "parsing"
              (test-case "num" (check-equal? (parse 10) (num 10)))
              (test-case "add" (check-equal? (parse '(+ 10 20)) (binop 'add (num 10) (num 20))))
              (test-case "sub" (check-equal? (parse '(- 10 20)) (binop 'sub (num 10) (num 20))))
              (test-case "mul" (check-equal? (parse '(* 10 20)) (binop 'mul (num 10) (num 20))))
              (test-case "div" (check-equal? (parse '(/ 10 20)) (binop 'div (num 10) (num 20))))
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
                                             (binop 'mul (num 20) (num 30)))))
              ))

;; Expressible Values

;; Expressible values are those that are returned as the result of
;; evaluating an expression.  For the ARITHMETIC language,
;; expressible values are numbers and booleans. 

(require racket/contract)
(define expressible-value? (or/c number? boolean?))

;; Error domain
;;
;; The interpreter receives an AST, and produces a number, boolean,
;; or throws an error.  We first define the types of errors it can
;; throw.
;; 
;; Errors raised by the interpreter
;;
;; Like before, errors are specialised exceptions.  We are
;; concerned with two kinds of exceptions raised during evaluation,
;; or execution time: divide-by-zero and type-error.
;; 
;; =exec-divide-by-zero= is raised when the numerator of a division is
;; zero.  =exec-type-error= is raised when there is an argument type
;; mismatch, e.g., a non-boolean value to the test of a
;; conditional, or a boolean argument to addition, etc.
  (struct exn:exec-div-by-zero exn:fail ())
  (define raise-exec-div-by-zero
    (lambda ()
      (raise (exn:exec-div-by-zero "div-by-0!" (current-continuation-marks)))))

  (struct exn:exec-type-mismatch exn:fail ())
  (define raise-exec-type-mismatch
    (lambda ()
      (raise (exn:exec-type-mismatch "type mismatch!" (current-continuation-marks)))))

  (struct exn:exec-not-implemented exn:fail ())
  (define raise-exec-not-implemented
    (lambda ()
      (raise (exn:exec-div-by-zero "not implemented!" (current-continuation-marks)))))

;; These errors are raised as follows:
;;   - =(raise-exec-div-by-zero)=
;;   - =(raise-exec-type-mismatch)=
;; 
;; Your interpreter will be expected to raise the above exceptions
;; under the appropriate conditions.
;; 
;; ** Runtime checks for types
;; The functions =typecheck-num=, =typecheck-bool= and
;; =check-non-zero= defined below check whether a value has the
;; right type and raise the appropriate runtime evaluator
;; exceptions.

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
      [_ error "unknown op"])))

;; ** =eval-ast= (You need to implement this)
;; =eval-ast= takes an AST and returns an expressible value. It
;; uses the =case= construct from =eopl= to perform pattern
;; matching against the =ast= abstract data type. It wraps
;; appropriate expressions with exception handlers to check for
;; division by zero and type mismatches.
;;; eval-ast :: ast? -> expressible? || (or/c exn:exec-div-by-zero  exn:exec-type-mismatch)
(define eval-ast
  (lambda (a)
    (cases ast a
      ;;    1. =binop= :: [binop? ast? ast?] -> ast?
      (binop (op rand1 rand2)
             (let ([r1 (typecheck-num (eval-ast rand1))]
                   [r2 (typecheck-num (eval-ast rand2))])
               (if (eq? op 'div) ((op-interpretation op) r1 (check-non-zero r2))  ((op-interpretation op) r1 r2))
               ))
      ;;    2. =ifte=  :: [ast? ast? ast?]   -> ast?
      (ifte (c t e)
            (let ([rc (typecheck-bool (eval-ast c))])
              (if rc (eval-ast t) (eval-ast e))
              ))
      ;;    3. =num=   :: number? -> ast?
      (num (n) (typecheck-num n))
      ;;    4. =bool=  :: boolean? -> ast?
      (bool (b) (typecheck-bool b))
      )))

;; ** Testing =eval-ast=
;; 
;; *** Routine test cases
(define ts-evaluation
  (test-suite
    "evaluation"
    (test-case "num" (check-equal? (eval-ast (num 10)) 10))
    (test-case "add" (check-equal? (eval-ast (binop 'add (num 10) (num 20))) 30))
    (test-case "sub" (check-equal? (eval-ast (binop 'sub (num 10) (num 20))) -10))
    (test-case "mul" (check-equal? (eval-ast (binop 'mul (num 10) (num 20))) 200))
    (test-case "lt" (check-equal? (eval-ast (binop 'lt? (num 10) (num 20))) #t))
    (test-case "eq" (check-equal? (eval-ast (binop 'eq? (num 10) (num 10))) #t))
    (test-case "div-success" (check-equal? (eval-ast (binop 'div (num 20) (num 10))) 2))
    ;; raise an exception, so use the correct `raise' function!
    (test-case "div-failure"
               (check-exn exn:exec-div-by-zero?
                          (lambda () (eval-ast (binop 'div (num 20) (num 0))) 2)))
    (test-case "bool-t" (check-equal? (eval-ast (bool #t)) #t))
    (test-case "bool-f" (check-equal? (eval-ast (bool #f)) #f))
    (test-case "if-true" (check-equal? (eval-ast (ifte (bool #t) (num 10) (num 20))) 10))
    (test-case "if-false" (check-equal? (eval-ast (ifte (bool #f) (num 10) (num 20))) 20))
    (test-case "if-type-mismatch"  (check-exn exn:exec-type-mismatch?
               (lambda () (eval-ast (ifte (num 42) (num 10) (num 20))))))))

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
                                     (num 10)))))))))

;; *** Testing Incorrect rand2 type
(define ts-numop-incorrect-param-rand2
  (test-suite
   "wrongly typed rand2 parameters"
   (for/list ([numerical-op '(add sub mul div)])
     (test-case (string-append (symbol->string numerical-op) "-type-mismatch-rand1")
       (check-exn exn:exec-type-mismatch?
                  (lambda () 
                    (eval-ast (binop numerical-op (num 10)
                                     (binop 'lt? (num 10) (num 20))))))))))


;; * Test Runners
;; 
;;   These run the tests that have been written in this file.  When
;;   submitting, please ensure that all these tests pass.

(define run-all-tests 
  (lambda ()
    (run-tests ts-parsing)
    (run-tests ts-evaluation)
    (run-tests ts-numop-incorrect-param-rand1)
    (run-tests ts-numop-incorrect-param-rand2)))

;; * Running the test suite
;; 
;;   We will use [[https://docs.racket-lang.org/raco/index.html][raco]] command line utility to run the tests.
;; 
;;   =raco test main.rkt= will run the test suite.

(module+ test
  (run-all-tests))
