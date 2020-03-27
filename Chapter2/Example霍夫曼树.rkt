#lang racket
  #|利用霍夫曼树来表示不定长数据编码，要找到一个树中包含的字符，
  可以通过从根开始不断向左或右子树寻找，直到找到要找的字符，把操
  作中的向左定义为0，向右定义为1，这样就可以把每个字符设定为不定
  长的由0，1组成的编码。可以将常用字符的权重加大放在上层，不常用
  的权重设小，放在下层，这样就可以令编码总长度尽量减小，那么找到
  一棵最优霍夫曼树就是使编码的效率最高。|#

  #|生成一颗最优霍夫曼树的方法：
        目的：使权重最高的叶离根节点距离最近，反之则远。
        方法：把所有叶看作一个集合，找出集合中权重最小的两个叶组
        成一个子树的两个叶，在之后的操作中将该子树看作一个权重为
        两个节点权重之和的一个节点，再进行同样的操作，直到最后只剩
        一个节点，生成完毕。|#

  #|霍夫曼树的定义
    霍夫曼树的叶的定义为一个list,其中的元素有symbol leaf,代表
其含义的符号，还有该符号的权重(如字符，出现的频率)。
    而通常情况霍夫曼树的定义为一个左子树和一个右子树，以及一个
包含树中所含元素的集合。还有树的总权重。|#
(define (make-leaf symbol weight)
    (list `leaf symbol weight))
(define (leaf? object)
    (= (car object) `leaf))
(define (symbol-leaf x)
    (cadr x))
(define (weight-leaf x)
    (caddr x))
(define (make-code-tree left right)
    (list left right 
                    (append (symbols left) (symbols right))
                    (+ (weight left) (weight right))))
(define (symbols tree)
    (if (leaf? tree)
        (list (car tree))
        (caddr tree)))
(define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))
(define (decode bits tree)
    (define (decode-1 bits current-tree)
        (if (null? bit)
            nil
            (let ((next-branch) 
                        (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch) (decode-1 (cdr bits) next-branch))
                    (decode-1 (cdr bits) next-branch))))
    (decode-1 bits tree))
(define (choose-branch bit current-branch)
    (cond ((= bit 0) (left-branch current-branch))
        ((= bit 1) right-branch current-branch)
        (else (error "bad bit --Choose-Branch" bit))))

#|由加权后的元素组成的集合：由于在组成树的过程中，非叶节点由
一组字符数组和权重组成，在这个过程中需要不停的将最小的两个元
素合并为一个元素，所以将集合表示为按权重排序的有序是最有效率
的。|#

(define (adjoin-set2 x set)
    (cond ((null? set) (list x))
        ((< (weight x)) (weight (car set)) (cons x set))
        (else (cons (car set) (adjoin-set2 x (cdr set))))))
    
(define (make-leaf-set pairs)
    (if (null? pairs)
        nil
        (let ((pair (car pairs)))
            (adjoin-set2 (make-leaf (car pair) (cadr pair)) 
                                                    (make-leaf-set (cdr pairs))))))
