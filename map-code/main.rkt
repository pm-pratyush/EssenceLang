#lang racket
(require rackunit)

;;;
;;; DO NOT CHANGE THIS
;;;

;;; fixed-point? : [F:[X -> X], x:X] -> boolean?
(define fixed-point?
  (lambda (F x)
    (equal? (F x) x)))


;;; mapcode : [init:[A -> X], F:[X -> X], done:[X -> B]] -> B
(define mapcode
  (lambda (init F done) ;; \rho, F, \pi
    (lambda (a)
      (letrec ([loop
		(lambda (x)
		  (if (fixed-point? F x)
		      (done x)
		      (loop (F x))))])
	(loop (init a))))))

;;;
;;; ALL CHANGES SHOULD BE AFTER THIS
;;;

;;; Factorial Example:

;;; !: nat? -> nat?
(define !
  (mapcode
   ;; init
   (lambda (n)
     (list n 1))
   ;; next
   (lambda (x)
     (match x
       [(list 0 a) x]
       [(list i a) (list (sub1 i) (* a i))]))
   ;; done
   second)
  )

;;; Rackunit test cases
(check-eq? (! 0) 1 "!0")
(check-eq? (! 2) 2 "!2")
(check-eq? (! 3) 6 "!3")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bubble Sort
(define (bubble-pass lst n)
  (if (or (eq? n 0) (eq? n 1))
      lst
      (let* ([a (car lst)]
             [b (car (cdr lst))]
             [rest (cdr (cdr lst))])
        (if (> a b)
            (cons b (bubble-pass (cons a rest) (sub1 n)))
            (cons a (bubble-pass (cons b rest) (sub1 n)))
            )
        )
      )
  )

(define bubble-sort
  (mapcode
   ;; init
   (lambda (lst)
     (list lst (length lst)))
   ;; next
   (lambda (x)
     (match x
       [(list lst 0) x]
       [(list lst 1) x]
       [(list lst n) (list (bubble-pass lst n) (sub1 n))]))
   ;; done
   first)
  )

;;; Rackunit test cases
(check-equal? (bubble-sort '()) '() "Bubble Sort - TC1")
(check-equal? (bubble-sort '(7)) '(7) "Bubble Sort - TC2")
(check-equal? (bubble-sort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10) "Bubble Sort - TC3")
(check-equal? (bubble-sort '(4 3 6 4 3 2 6 5 9 8 7 0 1 2 3 4 2 3 1 1 3 4 5 5 5 5 5 5 5 5 5)) '(0 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 7 8 9) "Bubble Sort - TC4")
(check-equal? (bubble-sort '(1 4 2 7 3 4 4 1 5 1 61 56 7 18 5 201 10 1 21 6 7 19 10 9 2 1 23 67 8 4 3 5 14 25 12)) '(1 1 1 1 1 2 2 3 3 4 4 4 4 5 5 5 6 7 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) "Bubble Sort - TC5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Heap Sort
(define (swap-elements lst i j)
  (let* ([temp (list-ref lst i)]
         [lst1 (list-set lst i (list-ref lst j))]
         [lst2 (list-set lst1 j temp)])
    lst2
    )
  )
  
(define (heapify lst n i)
  (let* ([largest i]
         [left (* 2 i)]
         [right (+ (* 2 i) 1)]
         [lst-len (length lst)])
    
    (when (and (< left n) (> (list-ref lst left) (list-ref lst largest)))
      (set! largest left))
    (when (and (< right n) (> (list-ref lst right) (list-ref lst largest)))
      (set! largest right))

    (if (not (equal? i largest))
        (heapify (swap-elements lst i largest) n largest)
        lst
        )
    )
  )

(define heap-sort
  (mapcode
   ;; init
   (lambda (lst)
     (list 'b lst '() (length lst) (quotient (length lst) 2)))
   ;; next
   (lambda (x)
     (match x
       [(list 'b u-lst s-lst n i)
        (if (equal? i -1)
            (list 'e u-lst s-lst n (- n 1))
            (list 'b (heapify u-lst n i) s-lst n (- i 1))
            )]
       [(list 'e u-lst s-lst n i)
        (if (equal? i -1)
            x
            (let ([f (car u-lst)])
              (list 'e (heapify (swap-elements u-lst 0 i) i 0) (append (list f) s-lst) n (- i 1))
              )
            )]
       )
     )
   ;; done
   third)
  )

;;; Rackunit test cases
(check-equal? (heap-sort '()) '() "Heap Sort - TC1")
(check-equal? (heap-sort '(7)) '(7) "Heap Sort - TC2")
(check-equal? (heap-sort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10) "Heap Sort - TC3")
(check-equal? (heap-sort '(4 3 6 4 3 2 6 5 9 8 7 0 1 2 3 4 2 3 1 1 3 4 5 5 5 5 5 5 5 5 5)) '(0 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 7 8 9) "Heap Sort - TC4")
(check-equal? (heap-sort '(1 4 2 7 3 4 4 1 5 1 61 56 7 18 5 201 10 1 21 6 7 19 10 9 2 1 23 67 8 4 3 5 14 25 12)) '(1 1 1 1 1 2 2 3 3 4 4 4 4 5 5 5 6 7 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) "Heap Sort - TC5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Selection Sort
(define inf 999999999999)
(define (find-min lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) (car lst)]
    [(let* ([a (car lst)]
            [b (find-min (cdr lst))])
       (if (< a b) a b)
       )]
    )
  )

(define (remove-elem elem lst)
  (cond
    [(null? lst) '()]
    [(equal? (car lst) elem) (cdr lst)] 
    [else (cons (car lst) (remove-elem elem (cdr lst)))]
    )
  )

(define selection-sort
  (mapcode
   ;; init
   (lambda (lst)
     (list lst '()))
   ;; next
   (lambda (x)
     (match x
       [(list '() s-lst) x]
       [(list u-lst s-lst)
        (let* ([min-elem (find-min u-lst)]
               [mod-u-lst (remove-elem min-elem u-lst)]
               [mod-s-lst (append s-lst (list min-elem))])
          (list mod-u-lst mod-s-lst)
          )
        ]
       )
     )
   ;; done
   second)
  )

;;; Rackunit test cases
(check-equal? (selection-sort '()) '() "Selection Sort - TC1")
(check-equal? (selection-sort '(7)) '(7) "Selection Sort - TC2")
(check-equal? (selection-sort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10) "Selection Sort - TC3")
(check-equal? (selection-sort '(4 3 6 4 3 2 6 5 9 8 7 0 1 2 3 4 2 3 1 1 3 4 5 5 5 5 5 5 5 5 5)) '(0 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 7 8 9) "Selection Sort - TC4")
(check-equal? (selection-sort '(1 4 2 7 3 4 4 1 5 1 61 56 7 18 5 201 10 1 21 6 7 19 10 9 2 1 23 67 8 4 3 5 14 25 12)) '(1 1 1 1 1 2 2 3 3 4 4 4 4 5 5 5 6 7 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) "Selection Sort - TC5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Merge Sort
(define (second-last lst)
  (cond
    [(null? lst) '()]
    [(null? (cdr lst)) '()]
    [else (car (cdr (reverse lst)))]
    )
  )

(define (remove-top lst)
  (if (null? lst)
      '()
      (reverse (cdr (reverse lst)))
      )
  )

(define (merge-sorted lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(<= (car lst1) (car lst2))
     (cons (car lst1) (merge-sorted (cdr lst1) lst2))]
    [else
     (cons (car lst2) (merge-sorted lst1 (cdr lst2)))]))

(define (merge lst l1 r1 l2 r2)
  (cond
    [(or (> l1 r1) (> l2 r2)) lst]
    [else
     (let* ([l-rest (if (> l1 0) (take (drop lst 0) (+ 1 (- (- l1 1) 0))) '())]
            [r-rest (if (< r2 (- (length lst) 1)) (take (drop lst (+ 1 r2)) (+ 1 (- (- (length lst) 1) (+ 1 r2)))) '())]
            [l (take (drop lst l1) (+ 1 (- r1 l1)))]
            [r (take (drop lst l2) (+ 1 (- r2 l2)))])
       (append (append l-rest (merge-sorted l r)) r-rest)
       )]
    )
  )

(define merge-sort
  (mapcode
   ;; init
   (lambda (lst)
     (list lst (list (list 'u 0 (sub1 (length lst))))))
   ;; next
   (lambda (x)
     (match x
       [(list '() stack) x]
       [(list lst (list (list 's 0 n))) x]
       [(list lst stack)
        (let* ([f-top (last stack)]
               [s-top (second-last stack)])
          (cond
            [(equal? 'u (car f-top))
             (match f-top
               [(list 'u l r)
                (if (equal? l r)
                    (list lst (append (remove-top stack) (list (list 's l l))))
                    (list lst (append stack (list (list 'u l (quotient (+ l r) 2)))))
                    )
                ]
               )
             ]
            [else
             (match s-top
               [(list 's l2 r2)
                (let ([l1 (second f-top)][r1 (third f-top)])
                                  (list (merge lst l2 r2 l1 r1) (append (remove-top (remove-top stack)) (list (list 's l2 r1))))
                                  )]
               [(list 'u l2 r2)
                (let ([l1 (second f-top)][r1 (third f-top)])
                  (if (and (equal? l1 l2) (equal? r1 r2))
                      (list lst (append (remove-top (remove-top stack)) (list (list 's l2 r2))))
                      (list lst (append stack (list (list 'u (add1 r1) r2))))
                      )
                  )]
               )
             ]
            )
          )
        ]
       )
     )
   ;; done
   first)
  )

;;; Rackunit test cases
(check-equal? (merge-sort '()) '() "Merge Sort - TC1")
(check-equal? (merge-sort '(7)) '(7) "Merge Sort - TC2")
(check-equal? (merge-sort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10) "Merge Sort - TC3")
(check-equal? (merge-sort '(4 3 6 4 3 2 6 5 9 8 7 0 1 2 3 4 2 3 1 1 3 4 5 5 5 5 5 5 5 5 5)) '(0 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 7 8 9) "Merge Sort - TC4")
(check-equal? (merge-sort '(1 4 2 7 3 4 4 1 5 1 61 56 7 18 5 201 10 1 21 6 7 19 10 9 2 1 23 67 8 4 3 5 14 25 12)) '(1 1 1 1 1 2 2 3 3 4 4 4 4 5 5 5 6 7 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) "Merge Sort - TC5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Quick Sort
(define (find-r lst l r)
  (define (find-localpos lst)
    (cond
      [(null? lst) 0]
      [else
       (let* ([pivot (car lst)])
         (length (filter (lambda (x) (< x pivot)) (cdr lst)))
         )
       ]
      )
    )
  (let* ([sublst (take (drop lst l) (+ 1 (- r l)))])
    (+ l (find-localpos sublst)))
  )

(define (quick-modify lst l r)
  (define (partition lst)
    (cond
      [(null? lst) '()]
      [else (let* ([pivot (car lst)]
                   [less (filter (lambda (x) (< x pivot)) (cdr lst))]
                   [greater (filter (lambda (x) (>= x pivot)) (cdr lst))]
                   )
              (append less (list pivot) greater))]
      )
    )
  (let* ([l-rest (if (> l 0) (take (drop lst 0) (+ 1 (- (- l 1) 0))) '())]
         [r-rest (if (< r (- (length lst) 1)) (take (drop lst (+ 1 r)) (+ 1 (- (- (length lst) 1) (+ 1 r)))) '())]
         [sublst (take (drop lst l) (+ 1 (- r l)))])
    (append (append l-rest (partition sublst)) r-rest)
    )
  )

(define quick-sort
  (mapcode
   ;; init
   (lambda (lst)
     (list lst (list (list 'u 0 (sub1 (length lst))))))
   ;; next
   (lambda (x)
     (match x
       [(list '() stack) x]
       [(list lst (list (list 'p 0 n))) x]
       [(list lst stack)
        (let* ([f-top (last stack)]
               [s-top (second-last stack)])
          (cond
            [(equal? 'u (car f-top))
             (match f-top
               [(list 'u l r)
                (if (equal? l r)
                    (list lst (append (remove-top stack) (list (list 'p l l))))
                    (let ([nr (find-r lst l r)]) (list (quick-modify lst l r) (append stack (list (list 'u l nr)))))
                    )
                ]
               )
             ]
            [else
             (match s-top
               [(list 'p l2 r2)
                (let ([l1 (second f-top)][r1 (third f-top)])
                                  (list lst (append (remove-top (remove-top stack)) (list (list 'p l2 r1))))
                                  )]
               [(list 'u l2 r2)
                (let ([l1 (second f-top)][r1 (third f-top)])
                  (if (and (equal? l1 l2) (equal? r1 r2))
                      (list lst (append (remove-top (remove-top stack)) (list (list 'p l2 r2))))
                      (list lst (append stack (list (list 'u (+ 1 r1) r2))))
                      )
                  )]
               )
             ]
            )
          )
        ]
       )
     )
   ;; done
   first)
  )

;;; Rackunit test cases
(check-equal? (quick-sort '()) '() "Quick Sort - TC1")
(check-equal? (quick-sort '(7)) '(7) "Quick Sort - TC2")
(check-equal? (quick-sort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10) "Quick Sort - TC3")
(check-equal? (quick-sort '(4 3 6 4 3 2 6 5 9 8 7 0 1 2 3 4 2 3 1 1 3 4 5 5 5 5 5 5 5 5 5)) '(0 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 7 8 9) "Quick Sort - TC4")
(check-equal? (quick-sort '(1 4 2 7 3 4 4 1 5 1 61 56 7 18 5 201 10 1 21 6 7 19 10 9 2 1 23 67 8 4 3 5 14 25 12)) '(1 1 1 1 1 2 2 3 3 4 4 4 4 5 5 5 6 7 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) "Quick Sort - TC5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Insertion Sort
(define (insert-elem elem s-lst)
    (cond
      [(null? s-lst) (list elem)]
      [(<= elem (car s-lst)) (cons elem s-lst)]
      [else (cons (car s-lst) (insert-elem elem (cdr s-lst)))]
      )
  )

(define insertion-sort
  (mapcode
   ;; init
   (lambda (lst)
     (list lst '()))
   ;; next
   (lambda (x)
     (match x
       [(list '() s-lst) x]
       [(list u-lst s-lst)
        (let* ([mod-u-lst (cdr u-lst)]
               [mod-s-lst (insert-elem (car u-lst) s-lst)])
          (list mod-u-lst mod-s-lst)
          )
        ]
       )
     )
   ;; done
   second)
  )

;;; Rackunit test cases
(check-equal? (insertion-sort '()) '() "Insertion Sort - TC1")
(check-equal? (insertion-sort '(7)) '(7) "Insertion Sort - TC2")
(check-equal? (insertion-sort '(10 9 8 7 6 5 4 3 2 1)) '(1 2 3 4 5 6 7 8 9 10) "Insertion Sort - TC3")
(check-equal? (insertion-sort '(4 3 6 4 3 2 6 5 9 8 7 0 1 2 3 4 2 3 1 1 3 4 5 5 5 5 5 5 5 5 5)) '(0 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 7 8 9) "Insertion Sort - TC4")
(check-equal? (insertion-sort '(1 4 2 7 3 4 4 1 5 1 61 56 7 18 5 201 10 1 21 6 7 19 10 9 2 1 23 67 8 4 3 5 14 25 12)) '(1 1 1 1 1 2 2 3 3 4 4 4 4 5 5 5 6 7 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) "Insertion Sort - TC5")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Binary Search -> Returns answer in 0-based indexing
(define binary-search
  (mapcode
   ;; init
   (lambda (a)
     (let *([lst (car a)]
            [target (car (cdr a))])
       (list lst target 0 (sub1 (length lst)) null)
      )
     )
   ;; next
   (lambda (x)
     (match x
        [(list lst target left right #f) x]
        [(list lst target left right #t) x]
        [(list lst target left right ans)
          (if (> left right)
              (list lst target left right #f)
              (let*([mid (quotient (+ left right) 2)]
                     [mid-val (list-ref lst mid)])
                 (cond
                   [(equal? mid-val target) (list lst target left right #t)] 
                   [(< mid-val target) (list lst target (add1 mid) right ans)]
                   [else (list lst target left (sub1 right) ans)]
                 ))
          )]
       ))
   ;; done
   fifth)
  )

;;; Rackunit test cases
(check-eq? (binary-search (list '() 3)) #f "Binary Search - TC1")
(check-eq? (binary-search (list '(1 2 3 4 5) 3)) #t "Binary Search - TC2")
(check-eq? (binary-search (list '(1 2 3 4 5) 6)) #f "Binary Search - TC3")
(check-eq? (binary-search (list '(1 1 1 1 1 2 2 3 3 4 4 4 4 4 5 5 5 5 5 5 6 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) 11)) #f "Binary Search - TC5")
(check-eq? (binary-search (list '(1 1 1 1 1 2 2 3 3 4 4 4 4 4 5 5 5 5 5 5 6 7 7 8 9 10 10 12 14 18 19 21 23 25 56 61 67 201) 21)) #t "Binary Search - TC5")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Depth First Search
;;; Hint: Is the stack of a program execution part of its state?

;;; Assumptions: The input should be an adjacency list, a starting node, and a node you are searching for. The final state will be true/false, outputing if you found the node or not.

(define depth-first-search
  (mapcode
   ;; init
   (lambda (a)
     (let *([graph (car a)]
            [s (car (cdr a))]
            [t (car (cdr (cdr a)))]
            )
       (list graph s t '() (list s) null)
      )
     )
   ;; next
   (lambda (x)
     (match x
       [(list graph s t vis stack ans)
        (cond
          [(equal? ans #t) x]
          [(equal? ans #f) x]
          [(equal? graph null) (list graph s t vis stack #f)]
          [(equal? stack null) (list graph s t vis stack #f)]
          [(or (> s (length graph)) (> t (length graph))) (list graph s t vis stack #f)]
          [else
           (let ([top (last stack)])
             (if (not (member top vis))
                 (if (equal? top t)
                     (list graph s t (cons top vis) stack #t)
                     (list graph s t (cons top vis)
                           (let ([u-stack (remove-top stack)]
                                 [neighbor (list-ref graph (sub1 top))])
                             (append u-stack neighbor)
                             )
                           ans)
                     )
                 (list graph s t vis (remove-top stack) ans)
                 )
             )]
          )
        ]
       ))
   ;; done
   sixth)
  )

;;; Rackunit test cases
(check-equal? (depth-first-search (list '() 1 2)) #f "Depth First Search - TC1")
(check-equal? (depth-first-search (list (list `(2) `(4)) 1 5)) #f "Depth First Search - TC2")
(check-equal? (depth-first-search (list (list `(2) `(4) `() `()) 1 4)) #t "Depth First Search - TC3")
(check-equal? (depth-first-search (list (list `(2 4) `(1 3 5) `(2 5) `(1 5) `(2 3 4)) 1 5)) #t "Depth First Search - TC4")
(check-equal? (depth-first-search (list (list `(2 4) `(3) `(2 5) `(1 5) `(2 3)) 2 4)) #f "Depth First Search - TC5")