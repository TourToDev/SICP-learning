#lang racket
#|要表示多项式，我们多项式看成一个符号排列的形式。
  假设一个数据结构poly，由一个变量和一个项的集合组成
    选择器:
        variable:取出该(单变量)多项式的变量
        term-list:取出该多项式的项集合
    构造器:
        make-poly:由一个指定变量和一组项集合构造一个多项式
        (make-term order coefficient):构造一个项，由阶数和系数组成
        (adjoin-terms t1 t2):将t1和t2两个项合并
        |#
    




(define (install-polynomial-package)
    (define (make-poly variable term-list) (cons variable term-list))
    (define (variable p)(car p))
    (define (term-list p)(cdr p))
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                            (add-terms (term-list p1)
                                            (term-list p2)))
            (error "Polys not in same var -- ADD-POLY"
                (list p1 p2))))
    (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                    (mul-terms (term-list p1) (term-list p2)))
         (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
    (define (tag p)
        (attach `polynomial p))
    (put `add `(polynomial polynomial)
                (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put `mul `(polynomial polynomial)
                (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put `make-poly `polynomial
                    (lambda (var term) (cons var term))))

(define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else 
            (let ((t1 (first-term L1))
                    (t2 (first-term L2)))
                    (cond ((> t1 t2) (make-term t1 (add-term (rest-term L1) L2)))
                        ((< t1 t2 ) (make-term t2 (add-term L1 (rest-term L2))))
                        (else 
                            (adjoin-term 
                                (make-term (order t1) (add (coeff t1) (coeff t2)))
                                (add-terms (rest-terms t1) (rest-terms t2)))))))))

(define (mul-terms L1 L2)
    (if (empty-list L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term) L2))
                    (mul-terms (rest-term L1) (rest-term L2))))
(define (mul-term-by-all-terms t1 L)
    (if (empty-list L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
            (adjoin-terms (make-term (+ (order t1) (order t2) (mul (coeff t1) (coeff t2)))
                            (mul-term-by-all-terms t1 (rest-terms L)))))))