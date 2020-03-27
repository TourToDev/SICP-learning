#|通过定义集合的语言来定义集合，即描述集合所具有的操作：
    ·并集
    ·交集
    ·将一个元素加入一个集合
    ·判断一个元素是否在某个集合中
  从数据抽象的角度来说，只要和上述操作相一致的集合的定义
都是正确的。|#

#|表示法一：将集合看作无序的列表，列表中的不同元素只能出
现一次，空集即由空列表表示。
    在其中实现集合的语言的方法如下 |#

(define (element-of-set? x set)
    (cond ((null? set) false)
        ((equal? x (car x)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

        #|对于交集的操作，情况有
            ·两集合其中有一为空 -> nil
            ·集合1的car元素为集合2中的元素 -> 将（car 集合1）取出
            ·否则继续|#
(define (intersection set1 set2)
    (cond ((or (null? set1) (null? set2)) nil)
        ((element-of-set? (car set1) set2) 
                (cons (car set1) (intersection (cdr set1) set2)))
        (else (intersection (cdr set1) set2))))

#|表示法2：将集合看作有序列表，将集合中的元素按照某种规定规则
升序排列。
        集合的各种操作的表示法则有所改变|#

(define (element-of-set?2 x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection2 set1 set2)
    (if (((or (null? set1) (null? set2))))  
        nil
        (let ((x1 (car set1))
                (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection2 (cdr set1) (cdr set2))))
            ((< x1 x2) (intersection2 (cdr set1) set2))
            ((< x2 x1) (intersection2 set1 (cdr set2)))))))

#|表示法3：将集合看作二叉树，将集合中的每个元素都看作一个节点，
树的每个节点有三个接口：入口(接入节点本身的元素)，左入口(指向
左子树)，右入口(指向右子树)，左右子树都有可能是空的。
    只要保证对于每个节点，左入口指向的元素小于当前节点的元素，右
入口的元素大于当前节点元素，对于一个集合可以有不止一种表示方法。
|#

(define (entry tree)
    (car tree))
(define (left-branch tree)
    (cadr tree))
(define (right-branch tree)
    (caddr tree))
(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set?3 item set)
    (cond ((null? set) false)
        ((= item (entry set)) true)
        ((< item (entry set) (element-of-set?3 item (left-branch set))))
        ((< (entry set) item) (element-of-set?3 item (right-branch set)))))

(define (adjoin-set x set)
    (cond ((null? set) (make-tree x `() `()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set)))
        ((> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin x (right-branch set))))))

(define (lookup given-key set-of-records)
    (cond ((null? set-of records) false)
        ((equal? given-key (key set-of-records)) (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))



