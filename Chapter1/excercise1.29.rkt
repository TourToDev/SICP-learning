#lang racket
(define (cube x) (* x x x))
(define (sum term a next b)
    (if (> a b)
        0
        (+ (term a) (sum term (next a) next b))))


(define (SimpsonIntegral f a b n)
    (define h (/ (- b a) n))
    (define (y k) (f (+ a (* k h))))
    (define (next k)
        (+ k 1))
    (define (factor k)
        (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            (else 2)))

    (define (term k)
        (* (factor k) (y k)))

    (if (not (even? n))
        (error "n must be even ")
        (* (/ h 3) (sum term 0 next n)))               
        )
    

