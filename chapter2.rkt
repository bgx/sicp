#lang sicp
(#%require (only racket provide time))
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

;;;;;;;;;;;;;; Experimental Below ;;;;;;;;;;;;;;;;;;