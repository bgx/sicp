#lang sicp
(#%require (only racket provide time error))
(#%require (rename racket racket-list list))
;(#%require (rename racket fold-left foldl))
;(#%require (rename racket fold-right foldr))
(#%require (only "chapter1.rkt" sqrt square gcd average divides? prime?))

; 2.1.1
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (negative? d)
        (cons (- (/ n g)) (- (/ d g)))
        (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

; 2.1.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment p_start p_end)
  (cons p_start p_end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))
(define (midpoint-segment s)
  (let ((start (start-segment s))
        (end   (end-segment s)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

; 2.1.4
(define (make-interval lower upper) (cons lower upper))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (define (mi a b c d)
      (make-interval (* a b)
                     (* c d)))
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (if (negative? lx)
        (if (negative? ux)
            (if (negative? ly)
                (if (negative? uy)
                    (mi ux uy lx ly)  ; -- --
                    (mi lx uy lx ly)) ; -- -+
                (mi lx uy ux ly))     ; -- ++
            (if (negative? ly)
                (if (negative? uy)
                    (mi ux ly lx ly)  ; -+ --
                    (make-interval (min (* lx uy) (* ux ly))   ; -+ -+
                                   (max (* lx ly) (* ux uy))))
                (mi lx uy ux uy)))    ; -+ ++
        (if (negative? ly)
            (if (negative? uy)
                (mi ux ly lx uy)     ; ++ --
                (mi ux ly ux uy))    ; ++ -+
            (mi lx ly ux uy)))))     ; ++ ++

(define (print-interval i)
  (newline)
  (display "(")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display ")"))

; 2.2.1
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(define (reverse-iterative items)
  (define (reverse-iter itemsleft output)
    (if (null? itemsleft)
        output
        (reverse-iter (cdr itemsleft) (cons (car itemsleft) output))))
  (reverse-iter items '()))

(define (reverse-recursive items)
  (if (null? (cdr items))
      items
      (append (reverse-recursive (cdr items)) (list (car items)))))

(define reverse reverse-iterative)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (for-each proc items)
  (cond ((not (list? items)) (display "Invalid input: items must be a list")(newline))
        ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

; 2.2.2
(define (deep-reverse items)
  (cond ((not (list? items)) (display "Invalid input: items must be a list")(newline))
        ((null? items) items)
        ((pair? (car items))
            (append (deep-reverse (cdr items))
                    (list (deep-reverse (car items)))))
        (else
            (append (deep-reverse (cdr items))
                    (list (car items))))))

(define (fringe items)
  (cond ((null? items) items)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items)) (fringe (cdr items))))))

(define (tree-map proc tree)
  (map (lambda (sub-tree) 
         (if (pair? sub-tree) 
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (x) (cons (car s) x))
                     rest)))))

; 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; map and accumulate with append
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; 2.3.2

; (e2.56 & e2.57) vvv

; [deriv]e [exp]ression [var]iable
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation (base exp) (make-sum (exponent exp) -1))
           (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (make-sum-with-seq seq)
    (cond ((null? (cdr seq)) (car seq))
          ((pair? (cdr seq))
           (make-sum (car seq) (make-sum-with-seq (cdr seq))))
          (else (make-sum (car seq) (cdr seq)))))
(define (augend s) (make-sum-with-seq (cddr s)))
(define (make-sum a1 a2 . a3)
  (define (make-sum-2-items a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (if (null? a3)
      (make-sum-2-items a1 a2)
      (make-sum-with-seq (cons a1 (cons a2 a3)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (make-product-with-seq seq)
    (cond ((null? (cdr seq)) (car seq))
          ((pair? (cdr seq))
           (make-product (car seq) (make-product-with-seq (cdr seq))))
          (else (make-product (car seq) (cdr seq)))))
(define (multiplicand p) (make-product-with-seq (cddr p)))
(define (make-product m1 m2 . m3)
  (define (make-product-2-items m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (if (null? m3)
      (make-product-2-items m1 m2)
      (make-product-with-seq (cons m1 (cons m2 m3)))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

; (e2.56 & e2.57) ^^^

; 2.3.3
; see chapter2-2.3.3.rkt

; 2.3.4

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


;;;;;;;;;;;;;; Experimental Below ;;;;;;;;;;;;;;;;;;

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;> (decode sample-message sample-tree)
;{A D A B B C A}
