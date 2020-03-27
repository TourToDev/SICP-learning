#lang racket
(define (cons x y)
    (define (dispatch m)
        (cond ((= m 1) x)
            ((= m 0) y)))
    dispatch)

(define (car z)
    (z 1))

(define (cdr z)
    (z 0))