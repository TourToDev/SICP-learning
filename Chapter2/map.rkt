#lang racket
(define (scale-list items factor)
    (if (null? items)
        nil
        (cons (* (car items) factor) (scale-list (cdr items) factor))))

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items)) (map proc (cdr items)))))
    
(define (scale-list2 items factor)
    (map (lambda(x) (* x factor)) items))

(define (for-each proc list)
    (cond ((not (nulls list)) (proc (car list)) (for-each proc (cdr list)))))