#|类似于在处理数值数据时将procedure抽象为数据，
在处理复合数据时将处理复合数据的procedure抽象，要
我们改变过程的数据结构处理方式。
    |#

#|如要处理一个树结构，计算树中所有奇数叶的平方和的
程序(sum-odd-square tree)：
    主要思路：筛选出树中的奇数叶，再平方加和
    筛选过程中的几个分支
    ·若空树，值为0
    ·若为叶，且为奇数，则将该值平方
    ·若为叶，且为偶数，则值为0
    ·若为树，则对其左右子树继续执行sum-odd-square程序
    并将结果相加。
|#

(define (sum-odd-square tree)
    (cond ((null? tree) 0)
        ((not(pair? tree)) (if (odd? tree)
            (square tree)
            0))
        (else (+ (sum-odd-square (car tree)) (sum-odd-square (cdr tree)
        )))))

#|再例如要挑选出偶数斐波那契数并且组成list的程序
(even-fibs n)
    主要思路：筛选出斐波那契数列中的偶数元素并用cons
    组成list
    筛选过程:利用next函数设置起始斐波那契数k,目标到达
    斐波那契数n,
    ·若f为偶数，则创建car为f,cdr为(next k+1)的list
    ·若f为奇数，则不创建直接进入(next k+1)
    ·直到k大于n退出迭代
|#

(define (even-fibs n)
    (define (next k n)
        (if (k>n)
            nil
            (let ((f fibs k))
                ((if (even? f)
                    (cons f (next (+ k 1) n))
                    (next (+ k 1) n)))))))

#|比较这两个程序，可以看出其中的共性
程序1：
    ·枚举树中所有的叶
    ·筛选其中符合条件的叶片(奇数)
    ·对符合条件的叶片执行处理（平方）
    ·利用+号从0开始累积结果

程序2：
    ·从0到n列举整数
    ·计算每个整数的菲波纳契数
    ·筛选其中符合条件的斐波那契数(偶数)
    ·利用cons从空list开始积累结果

我们可以使用信号分阶段处理的方法将list当作信号进行处理，
    ·枚举器(enumerate)
    ·过滤器(filter)
    ·批处理(map)
    ·结果累计(accumulation)
|#

(define (filter predicate sequence)
    (cond ((null? sequence) 0)
        ((predicate (car sequence) (cons (car sequence)
            (filter predicate (cdr sequence)))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (low>high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
        ((not(pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) 
                    (enumerate-tree (cdr tree))))))

#|重新构建sum-odd-square函数：
    利用enumerate-tree函数将树的所有叶作为序列传入filter函数，
再将filter筛选出的序列批量平方后传入acccumulate函数
|#
(define (sum-odd-square2 tree)
    (accumulate + 0 
                (map square (filter odd?  
                                    (enumerate-tree tree)))))

#|重新构建even-fibs函数：
|#

(define (even-fibs2 n)
    (accumulate con nil 
                (filter even?
                        (map fib (enumerate-interval 0 n)))))


#|嵌套批量处理(Nested Mapping)
    例：给定一正整数n，找到所有符合 1<i<j<n，且i+j为素数的i,j组合。
        为了产生这样的组合的序列：
        ·列举每个小于n的整数i
        ·对于每个i列举所有小于i的整数j
        ·对于每个j，生成一个i,j组合(i,j)。
        由于map可以对传入的序列的每个元素进行处理，可以使用map的嵌套
        来实现类似多层循环的操作。
        ·先对于1-n的序列(enumerate 1 n)进行map设其变量为i，再在其中对
        1-i的序列(enumerate 1 i)进行map，执行(list i j)
        ·这样对于每个i都创建了(i,j)对，
        |#
(define (make-sequence n)
    (accumulate append nil
                        (map (lambda (i) 
                                (map ((lambda (j) (list i j)))
                                    (enumerate-interval 1 j)))
                                        (enumerate-interval 1 n))))
(define (flatmap proc seq)
    (accumulate append nil (map prc seq)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-pair-sum n)
    (map make-pair-sum 
                    (filter prime-sum? (flatmap 
                                        (lambda (i) (map
                                            (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
                                            (enumerate 1 n)))))

(define (pertu s)
    (if (null? s)
        (list nil)
        (flatmap (lambda (x) 
                        (map (lambda (p) (cons x p) (pertu (remove x s))))) s)))

(define (remove x s)
    (filter (lambda (item) (not (= item x)) s)))

(lambda (x y z) (+ x y z))