#lang racket
 (define (reverse items) 
   (define (iter items result) 
     (if (null? items) 
         result 
         (iter (cdr items) (cons (car items) result)))) 
  
   (iter items nil)) 

(define (deep-reverse tree)
    (cond ((null? tree) nil)
        ((not (pair? tree)) tree)
        (else 
            (reverse (list (deep-reverse (car tree)
                            (deep-reverse (cadr tree))))))))