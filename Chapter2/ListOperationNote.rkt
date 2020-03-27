#|列表操作1：连续的cdr下去，进而操作列表元素
        如将一个序列的第n个元素输出的函数list-ref:
        ·对于基例n=0，输出（car list）
        ·对于n不等于的0的，则输出cdr list（去除第一个元素子列表）的n-1个元素
        |#
    (define (list-ref list n)
        (if (= n 0)
            (car list)
            (list-ref (cdr list) (- n 1))))
#|列表操作2：当我们连续cdr的遍历一整个列表时，我们需要知道列表什么时候结束，
因为列表的最后一个cdr是空，nil，我们可以使用内置的null?函数来判
断是否为空列表。
        如将一个列表的长度输出的length函数：
        ·基例：空列表的长度为0
        ·对于非空列表，长度为1 + cdr list
        |#
(define (length list)
    (if (null? list)
        0
        (+ 1 (length (cdr list)))))

#|还可以引入一个计数器count来将其改造为迭代风格|#
(define (length2 list)
    (define (iter list count)
        (if (null? list)
            count
            (iter (cdr list) (+ 1 count)))))

#|列表操作3：当我们cdr遍历一个列表的同时利用cons创建一个答案列表。
        如将两个列表链接成一个列表的append函数：
        ·若第一个列表为空，则输出第二个列表（cons 1 list2可以将list2接在1后面生成新的列表）
        ·否则就对cdr list1和list2执行append操作，同时利用cons (car list1)将list1的
        元素取出形成一个新列表。
        |#
(define (append list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))))

#|高级抽象：对列表的批量操作(mapping over list)，对一个列表的每个元素
都执行某种运算然后产生结果列表。
        如将列表中每个元素都放大一定倍数的程序scale-list|#

    (define (scale-list items factor)
        (if (null? items)
            nil
            (cons (* items (car factor)) (scale-list (cdr items) factor))))

    (define (map proc items)
        (if (nulls? item)
            nil
            (cons (proc (car items)) (map proc (cdr items)))))
    
    (define (scale-list2 items factor)
        (map (lambda (x) (* x factor)) items))