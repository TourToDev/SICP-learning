#lang racket
#|牛顿法求一个数x的平方根，sqrt(x)|#
#|方法:1.作出一个猜测数 Guess,
    2.后将x除以Guess得到商 Quotient
    3.再求Quotien和Guess的平均值Average
    4.将Average当作新的Guess代入第二步，直到偏差abs(square(Guess)-x)
    小于我们的预定值|#
(define tolerance 0.00001)
(define (Average x y)
    (/ (+ x y) 2))
(define (GoodEnough guess x)
    ( < (abs (- (square guess) x) ) tolerance))
(define (square x)
    (* x x))
(define (improve guess x)
    (Average guess (/ x guess)))
(define (sqrt-iter guess x)
    (if (GoodEnough guess x)
        guess
        (sqrt-iter (improve guess x) x)))
    