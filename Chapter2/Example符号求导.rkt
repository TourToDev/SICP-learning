#lang racket
#|符号化求导程序：接受一个表达式exp，和对表达式求导的变量var
                wishful thinking:先设定求导过程的语言
                                variable? e 测试e是否为变量（lisp中为符号）
                                same-variable? v1 v2 测试v1,v2是否为相同变量
                                sum? e 测试e是否为加和
                                addend e augend e 分别取出加和e的第一和第二个参数
                                makesum a1 a2 构造 a1 和 a2的和
                                product? e 测试e是否为乘积
                                multiplier e multipliand e 
                                make-product m1 m2 构造 m1 m2的乘积
|#

(define (deriv exp var)
   (cond ((number? exp) 0)
       ((variable? exp) (if (same-variavle exp var)
           1
           0))
        ((sum? exp) 
            (make-sum (deriv (addend exp) var  
                                (deriv (augend exp)))))
        ((product? exp) (make-sum 
                            (make-product (multiplier exp)
                                            (deriv (multipliand exp) var))
                            (make-product (deriv (multiplier exp) var)
                                            (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))

(define (variable? x)
    (symbol? x))

(define (same-variable v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list `+ a1 a2)))

(define (make-product m1 m2)
    (list `* m1 m2))

(define (sum? x)
    (and (pair? x) (eq? (car x) `+)))

(define (product? x)
    (and (pair? x) (eq (car x) `*)))

(define (multiplier p)
    (cadr p))

(define (multiplicand p)
    (caddr p))


