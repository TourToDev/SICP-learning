#lang racket
(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref items (- n 1))))

(define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))

(define (length2 items)
    (define (length-iter a count)
        (if (null? a)
            count
            (length-iter (cdr a) (+ count 1))))
    (length-iter items 0))
(define (apppend list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (apppend (cdr list1) list2))))

(define (last-pair list)
    (list-ref list (- (length list) 1)))

(define nil '()) 

(define (reverse list)
    (define n (length2 list))
    (define (rev-iter list n)
        (if (= n 0)
            nil
            (cons (list-ref  list (- n 1)) (rev-iter list (- n 1)))))
  (rev-iter list n))

 (define (reverse2 items) 
   (define (iter items result) 
     (if (null? items) 
         result 
         (iter (cdr items) (cons (car items) result)))) 
  
   (iter items nil)) 

(define (cc amount coin-value)
    (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-value)) 0)
        (else 
            (+ (cc amount (except-first-denomination coin-value)
                (cc (- amount first-denomination) coin-value))))))

(define (no-more? coin-value)
    (null? coin-value))

(define (except-first-denomination coin-value)
    (cdr coin-value))
(define (first-denomination coin-value)
    (car coin-value))

(define (remain? x y)
    (if (= (remainder x y) 0)
        false
        true))
(define (same-parity x . list)  
    (if (null? list)
        x
        (if (= (apply (lambda (list) (remainder list x)) list) 0)
            (cons x (same-parity (car list) (cdr list)))
            (same-parity x list))))