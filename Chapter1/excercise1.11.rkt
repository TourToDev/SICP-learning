#lang racket
#|f(n)=3 if n<3 
  f(n)=f(n-1) + 2*(f(n-2)) + 3*f(n-3) if n>=3|#
(define (f1 n)
    (if (< n 3)
        n
        (+ (f1 (- n 1)) (f1 (* 2 (- n 2))) (f1 (* 3 (- n 3))))))

(define (f-iter a b c i n)
    (if (= i n)
        c
        (f-iter  (+ a (* 2 b) (* 3 c)) a b (+ i 1) n)))

(define (f2 n)
    (f-iter 2 1 0 0 n))