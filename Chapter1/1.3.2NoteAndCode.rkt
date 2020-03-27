#|用lambda定义过程|#
(define (pi-sum a b)
    (sum (lambda(x) (/ 1.0 (* x (+ x 2)))) 
            a
            (lambda (x) (+ x 1))
            b))

((lambda (x y z) (+ x y (sqare z)) 1 2 3))

#|用lambda创建本地变量
    如计算f(x,y)=x(1+xy)^2+y(1-y)+(1+xy)(1-y)
    可以令a=1+xy
        b=1-y
        f(x)=xa^2+yb+ab|#

(define (f x y)
    ((lambda (a b) (+ (* x (sqare a)) (* y b) (*a b))))
                (+ 1 (* x y))
                (- 1 y))

#|特殊形式let|#
(define (f x y)
    (let ((a (+ 1 (* x y)))
            (b (-1 y))))
            (+ (* x (square a))
                (* y b)
                (* a b)))    

