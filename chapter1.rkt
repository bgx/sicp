#lang sicp
(#%require (only racket provide time))
(#%require (rename racket racket-list list))

;;; Provides ;;;

; 1.1.4
(provide square sum-of-squares)

; 1.1.8
(provide sqrt)

; 1.2.1
(provide factorial-recursive factorial-iterative)
;(provide (rename-out [factorial-iterative factorial]))
(provide factorial)
;(provide ackermanns)

; 1.2.5
(provide gcd)

; 1.2.6
(provide divides? prime?)

; 1.3.4
(provide average)

;;; Defines ;;;

; 1.1.4
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

; 1.1.8
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

; 1.2.1
(define (factorial-recursive n)
  (if (= n 1)
      1
      (* n (factorial-recursive (- n 1)))))

(define (factorial-iterative n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define factorial factorial-iterative)

(define (ackermanns x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ackermanns (- x 1)
                          (ackermanns x (- y 1))))))

; 1.2.2
(define (fib-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-recursive (- n 1))
                 (fib-recursive (- n 2))))))

(define (fib-iterative n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; 1.2.3
(define (cube x) (* x x x))

(define (sine angle) ;angle in radians
  (define (p x)
    (- (* 3 x) (* 4 (cube x))))
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; 1.2.4
(define (even? n)
  (= (remainder n 2) 0))

(define (expt-fast-recursive b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-fast-recursive b (/ n 2))))
        (else (* b (expt-fast-recursive b (- n 1))))))

(define (expt-fast-iterative b n)
  (define (expt-fast-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (expt-fast-iter (* b b) (/ n 2) a))
          (else (expt-fast-iter b (- n 1) (* a b)))))
  (expt-fast-iter b n 1))

(define expt-fast expt-fast-iterative)

(define (fib-fast n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
               (fib-iter a
                         b
                         (+ (square p) (square q))
                         (+ (square q) (* 2 p q))
                         (/ count 2)))
          (else
               (fib-iter (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- count 1)))))
  (fib-iter 1 0 0 1 n))

(define fibonacci fib-fast)

; 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 1.2.6

; does a divide b?
(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (next x)
      (if (= x 2)
          3
          (+ x 2)))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

; 1.3.1
(define (sum-recursive term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recursive term (next a) next b))))

(define (sum-iterative term a next b)
  (define (sum-iter a result)
    (if (> a b)
        result
        (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define sum sum-iterative)

(define (product-iterative term a next b)
  (define (product-iter a result)
    (if (> a b)
        result
        (product-iter (next a) (* result (term a)))))
  (product-iter a 1))

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define product product-iterative)

; finite approximation to Wallis product
(define (pi-approx n)
  (define (term x)
    (let ((z (* 4.0 (square x))))
      (/ z
         (- z 1))))
   (* 2 (product term 1 inc n)))

; 1.3.3
(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (cont-frac-recursive n d k)
  (define (cont-frac-inner index)
    (if (< index k)
        (/ (n index)
           (+ (d index)
              (cont-frac-inner (+ index 1))))
        (/ (n index)
           (d index))))
  (cont-frac-inner 1))

(define (cont-frac-iterative n d k)
  (define (cont-frac-iter index result)
    (if (< index 1)
        result
        (cont-frac-iter (- index 1) (/ (n index)
                                       (+ (d index) result)))))
  (cont-frac-iter (- k 1) (/ (n k)
                              (d k))))

(define cont-frac cont-frac-iterative)

(define (golden-ratio-inverse-approx k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             k))

; 1.3.4
(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter g k)
    (if (> k 1)
        (repeated-iter (compose f g) (- k 1))
        g))
  (repeated-iter f n))

(define (smooth f)
  (define dx 0.001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

; Correct:   ( (smooth (smooth f)) x )
; Correct:   ( ((lambda (y) (smooth (smooth y))) f) x )
; Incorrect: ( ((smooth smooth) f) x )
(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(define (iterative-improve good-enough improve)
  (lambda (guess)
    (if (good-enough guess)
        guess
        ((iterative-improve good-enough improve) (improve guess)))))