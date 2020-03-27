#lang racket
#|用半区间法寻找方程的根：
        要寻找方程 f(x)=0 的根，f(x)为一连续函数，若给定两点a，b，在这两点
    f(a)>0,f(b)<0,则在a，b之间必存在至少一个0点，那么令x为a,b的平均值，若f(x)>0,
    则将区间改为a到x，若f(x)<0,则将区间改为x到a，这样不断缩小区间范围，直到达到一定
    精度。|#
(define (average x y)
    (/ (+ x y) 2))
(define (cube x)
  (* x x x))
(define (search f neg-point pos-point)
    (let ((midpoint (/ (+ neg-point pos-point) 2)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value) (search f neg-point midpoint))
                    ((negative? test-value) (search f midpoint pos-point))
                    (else midpoint))))))   


(define (close-enough? x y)
    (< (abs (- x y)) 0.00001))

#|寻找一个函数的定点：
        函数f(x)的定点为满足f(x)=x的点，为了找到这个点，先作出一个初始猜测，
    然后将其带入f，再将结果f(x)代入f，循环此过程，若前后两值足够接近，即满足
    其差别小于一定的值，即得到近似结果。
    |#


(define (fixed-point f first-guess)
    (let ((next (f first-guess)))
        ((if (close-enough first-guess)
            next
            (fixed-point f next)))))

#|可以用寻找定点的方法来寻找一个数x的平方根，它的平方根y满足
y=x/y,即给定一个数x，找到函数x/y的定点，就是x的平方根。但这个
方法并不收敛，会进入一个死循环，为了解决这个问题，利用平均阻尼
的方法，可以将其变化为y=(y+x/y)/2=average(y,(x/y)),即给定一个
函数f，我们考虑一个在x处值为x和f(x)的平均值的函数。|#
(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (x/y)) )1))