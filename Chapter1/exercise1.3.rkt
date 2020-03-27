#lang racket
(define (bigger a b) (if (< a b) b a))
(define (smaller a b) (if (< a b) a b))
(define (sumofsquare a b) (+ (* a a) (* b b)) )
(define (main a b c)
   (sumofsquare (bigger a b) (bigger (smaller a b) c)))