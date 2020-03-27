#lang racket
(define (average x y)
    (/ (+ x y) 2))
(define (average-damp f)
    (lambda (x) (average x (f x))))
 (define tolerance 0.00001) 
  
 (define (fixed-point f first-guess) 
   (define (close-enough? v1 v2) 
     (< (abs (- v1 v2)) tolerance)) 
   (define (try guess) 
     (let ((next (f guess))) 
       (if (close-enough? guess next) 
           next 
           (try next)))) 
   (try first-guess))

(define (repeated f n)
    (if (= n 1)
        f
        (lambda (x) (f ((repeated f (- n 1)) x)))))   
(define (expt base n)
    (if (= n 0)
        1
        ((repeated (lambda (x) (* base x)) n) 1)))
    
(define (average-damp-n-times f n)
    (repeated (average-damp) n) f)
(define (damped-nth-root n damp-times)
    (lambda (x) 
        (fixed-point (average-damp-n-times 
                        (lambda (y) (/ x (expt y (- n 1))))
                        damp-times)
                        1.0)))

(define (lg n)
    (cond ((> (/ n 2) 1) (+ 1 (lg(/ n 2))))
        ((< (/ n 2) 1) 0)
        (else 1)))

