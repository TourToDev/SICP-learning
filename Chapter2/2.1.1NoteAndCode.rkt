#|要构造一个有理数，并对有理数进行加减乘除运算，还有测试
两个有理数是否相等
    先假设对于一个有理数我们可以用 numer(x)返回他的分母，
denom(x)返回他的分子，而先不考虑一个有理数是怎么具体实现的，
根据有理数的加减乘除原则，可以写出各个过程的定义。|#
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom x))
                    (* (numer y) (denom y))) 
                    (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
    (let ((g (gcd n d)))
        (if (< d 0)
            (cons (/ (- n) g) (/ (- d) g))
            (cons (n/g) (d/g)))))
(define (numer x)
    (car x))
(define (denom x)
    (cdr x))
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

  