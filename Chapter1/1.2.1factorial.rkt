#lang racket
#|用两种过程实现求一个数x的阶乘，
    1.递归过程
    2.迭代过程|#
#|递归过程|#
(define (factorial1 x)
    (if (= x 1)
        1
        (* x (factorial1 (- x 1)))))
#|迭代过程|#
(define (factorial2 x)
    (fact-iter 1 1 x))

(define (fact-iter product counter x)
    (if (> counter x)
        product
        (fact-iter (* product counter) (+ counter 1) x)))
