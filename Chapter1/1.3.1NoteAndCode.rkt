#lang racket
#|计算从a到b的求和|#
(define (sum term a next b)
    (if (> a b)
        0   
        (+ (term a) 
            (sum term (next a) next b))))
(define (cube x)
    (* x x x)) 
(define (inc n)
    (+ n 1))
(define (cube-sum a b)
    (sum cube a inc b))    


(define (identity n)
    n)
(define (int-sum identity a inc b) 
    (sum identity a inc b))

(define (pi-term x)
    (/ 1 (* (+ x 2) x)))
(define (pi-next x)
    (+ x 4))
(define (pi-sum a b)
    (sum pi-term a pi-next b))


(define (integral f a b dx)
    (define (add-dx x)
    (+ x dx))
    (* (sum f a add-dx b) dx))