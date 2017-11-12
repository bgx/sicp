#lang sicp
(#%require (only racket provide time error random))
(#%require (rename racket racket-list list))
;(#%require (rename racket fold-left foldl))
;(#%require (rename racket fold-right foldr))
(#%require (only "chapter1.rkt" sqrt square gcd average divides? prime?))

; 3.1.1

(define (make-accumulator sum)
  (lambda (x)
    (set! sum (+ sum x))
    sum))

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) count)
            ((eq? x 'reset-count) (set! count 0))
            (else (set! count (+ count 1))
                  (f x))))))

; 3.3.1

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;;;;;;;;;;;;;; Experimental Below ;;;;;;;;;;;;;;;;;;

