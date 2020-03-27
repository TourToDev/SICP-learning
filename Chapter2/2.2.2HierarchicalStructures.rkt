#|层级结构，对于元素本身就是列表的列表来说，可以用树结构来进行表示，
如对于列表中一个不是列表的元素，可以将其看为一个叶片，对于本身也是
列表的元素可以看作子树。
    如对于(cons (list 1 2) (list 3 4)),就可以看作一颗树，其有一个
    子树，子树有两个叶片，除子树外还有两个叶。
|#

#|对于树的递归操作：总可以将对于树的操作分解为对其分支的操作，直到
达到最底层（叶）。
    如计算一个树的所有叶的程序count-leaves:
    ·基例：一个叶片的count-leaves为1
    ·对于树，树的count-leaves = 其第一个分支(car tree)的count-leaves + 除第一个分支外所有分支(cdr tree)
    的count-leaves|#

(define (count-leaves x)
    (cond ((null? x) 0)
            ((not (pair? x) 1))
            (else (+ (count-leaves (car x)) 
                        (count-leaves (cdr x))))))

#|对树进行批量操作(mapping over tree)|#
(define (scale-tree tree factor)
    (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor) (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree)
    (map (lambda (x) (if (pair? x)
        (scale-tree-map x)
        (* x factor))) tree))