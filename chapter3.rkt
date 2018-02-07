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

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;;;;;;;;;;;;; Experimental Below ;;;;;;;;;;;;;;;;;;

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and a1 a2)
  (cond ((= a1 0)
         (cond ((= a2 0) 0)
               ((= a2 1) 0)
               (else (error "Invalid a2 signal" a2))))
        ((= a1 1)
         (cond ((= a2 0) 0)
               ((= a2 1) 1)
               (else (error "Invalid a2 signal" a2))))
        (else (error "Invalid a1 signal" a1))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-or a1 a2)
  (cond ((= a1 0)
         (cond ((= a2 0) 0)
               ((= a2 1) 1)
               (else (error "Invalid a2 signal" a2))))
        ((= a1 1)
         (cond ((= a2 0) 1)
               ((= a2 1) 1)
               (else (error "Invalid a2 signal" a2))))
        (else (error "Invalid a1 signal" a1))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; (or A B) == (not (and (not A) (not B)))
;; Delay time of this or-gate is (and-gate-delay + 2*inverter-delay)
(define (or-gate2 a1 a2 output)
  (let ((x (make-wire))
        (y (make-wire))
        (z (make-wire)))
    (inverter a1 x)
    (inverter a2 y)
    (and-gate x y z)
    'ok))

;; a-wires, b-wires, s-wires should be ordered n to 1 (i.e. high to low)
(define (ripple-carry-adder a-wires b-wires s-wires c)
  (define (rca-iter a-wires b-wires c-in s-wires)
    (if (null? (cdr a-wires))
        (begin
          (full-adder (car a-wires) (car b-wires) c-in (car s-wires) c)
          'ok)
        (let ((c-out (make-wire)))
          (full-adder (car a-wires) (car b-wires) c-in (car s-wires) c-out)
          (rca-iter (cdr a-wires) (cdr b-wires) c-out (cdr s-wires)))))
  (let ((c-in (make-wire)))
    (set-signal! c-in 0)
    (rca-iter a-wires b-wires c-in s-wires)))

;; a-wires, b-wires, s-wires should be ordered 1 to n (i.e. low to high)
(define (ripple-carry-adder a-wires b-wires s-wires c)
  (define (rca-iter a-wires b-wires c-in s-wires c-out)
    (full-adder (car a-wires) (car b-wires) c-in (car s-wires) c-out)
    (if (null? (cdr a-wires))
        (begin
          (set-signal! c-in 0)
          'ok)
        (rca-iter (cdr a-wires) (cdr b-wires) (make-wire) (cdr s-wires) c-in)))
  (rca-iter a-wires b-wires (make-wire) s-wires c))


