(define nil (quote ()))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
        (if (null? items)
            0
            (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; 2.18
(define (reverse items)
  (define (iter lis result)
    (if (null? lis)
        result
        (iter (cdr lis) (cons (car lis) result))))
  (iter items nil))

;; 2.19
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)

(define except-first-denomination cdr)

(define no-more? null?)

;; 2.20
(define (same-parity x . y)
  (define (recur lis pred?)
    (cond
     ((null? lis) nil)
     ((pred? (car lis)) (cons (car lis) (recur (cdr lis) pred?)))
     (else
      (recur (cdr lis) pred?))))
  (cons x (recur y (if (odd? x)
                       odd?
                       even?))))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* factor x))
       items))

;; 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x))
       items))

;; 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
;; iter内でのconsで(square (car things))とanswerを引数として取っている．
;; この場，次のcdrでconsされるのは(square (car (cdr things))) と((square (car things)) answer)．
;; ここで順番が逆になっている．
;; このまま続けていくと欲しかったリストの逆順が返される．

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))
;; 始めのconsで作られるのは(() . 1)．
;; 次の繰り返しでconsすると((() . 1) . 2)ができる．
;; 始めのconsで作られたドット対を要素としたドット対ができる．
;; これを繰り返すのでうまくいかない．

;; 2.23
(define (for-each proc items)
  (cond
   ((null? items) 'done)
   (else
    (proc (car items))
    (for-each proc (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; 2.25
;; (1 3 (5 7) 9)
;; (car (cdr (car (cdr (cdr)))))

;; ((7))
;; (car (car ))

;; (1 (2 (3 (4 (5 (6 7))))))
;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ))))))))))))

;; 2.26
(define x (list 1 2 3))

(define y (list 4 5 6))

;; (append x y)
;; (1 2 3 4 5 6)

;; (cons x y)
;; ((1 2 3) 4 5 6)

;; (list x y)
;; ((1 2 3) (4 5 6))

;; 2.27
(define x (list (list 1 2) (list 3 4)))

(define (reverse items)
  (if (null? items)
      nil
      (append (reverse (cdr items)) (list (car items)))))

(define (reverse items)
  (define (iter lis result)
    (if (null? lis)
        result
        (iter (cdr lis) (cons (car lis) result))))
  (iter items nil))

(define (deep-reverse items)
  (cond ((null? items) nil)
        ((pair? items) (append (deep-reverse (cdr items))
                               (list (deep-reverse (car items)))))
        (else items)))

;; 2.28
(define x (list (list 1 2) (list 3 4)))

(define (fringe l)
  (cond ((null? l) nil)
        ((pair? (car l)) (append (fringe (car l))
                                 (fringe (cdr l))))
        (else (cons (car l) (fringe (cdr l))))))

;; 2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;; b
(define (total-weight mob)
  (if (pair? mob)
      (+ (total-weight (branch-structure (left-branch mob)))
         (total-weight (branch-structure (right-branch mob))))))

;; c
(define (balanced? mob)
  (if (pair? mob)
      (and (= (* (branch-length (left-branch mob))
                 (total-weight (left-branch mob)))
              (* (branch-length (right-branch mob))
                 (total-weight (right-branch mob))))
           (balanced? (left-branch mob))
           (balanced? (right-branch mob)))
      #f))

;; d
(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree subtree factor)
             (* sub-tree factor)))
       tee))

;; 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; 2.31
(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fn sub-tree)
             (fn sub-tree)))
       tree))

(define (sqaure x)
  (* x x))

(define (square-tree tree)
  (tree-map square tree))

;; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))
;; s = nil
;; ()

;; s = (3)
;; rest = ()
;; (cons (car s) x) = (3)

;; s = (2 3)
;; rest = (() (3))
;; (cons (car s) x) = (2) (2 3)

;; s = (1 2 3)
;; rest = (() (3) (2) (2 3))
;; (cons (car s) x) = (1) (1 3) (1 2) (1 2 3)

;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


(define (sum-odd-square tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-square (car tree))
                 (sum-odd-square (cdr tree))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (define (iter a b k)
    (if (= k n)
        a
        (iter b (+ a b) (+ 1 k))))
  (iter 0 1 0))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
             nil
             (filter even?
                     (map fib
                          (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (if (null? sequence)
      nil
      (cons (p (car sequence)) (map p (cdr sequence)))))

(define (length sequence)
  (if (null? sequence)
      0
      (+ 1 (length (cdr sequence)))))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

(define (horner-eval x coefficient-sequence)
  (define (iter lis res)
    (cond ((null? lis) res)
          (else (iter (cdr lis) (+ (car lis) (* x res))))))
  (iter (reverse coefficient-sequence) 0))

(define (horner-eval x coefficient-sequence)
  (cond ((null? coefficient-sequence) 0)
        (else (+ (car coefficient-sequence) (* x (horner-eval x (cdr coefficient-sequence)))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 2.35
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x) 1)
                       (enumerate-tree tree))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (cond ((null? x) 0)
                                         ((not (pair? x)) 1)
                                         (else (count-leaves x)))) t)))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))

(define (map proc items . rest)
  ;; 一つのリストのみに対応したmap
  (define (one-map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (one-map proc (cdr items)))))
  ;; 複数のリストに対応したmap
  (define (some-map proc items rest)
    (cond ((null? items) nil)
          ((null? rest) (car items))
          (else (cons (proc (car items)
                            ;; restのcarをitemsに，cdrをrestに入れて再帰
                            (some-map proc (car rest) (cdr rest)))
                      ;; restは複数のリストなのでcdrでなくone-mapでcdrする
                      (some-map proc (cdr items) (one-map cdr rest))))))
  ;; restの有無でone-mapかsome-mapに分岐
  (if (null? rest)
      (one-map proc items)
      (some-map proc items rest)))

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))

;; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

;; fold-rightとfold-leftによってopが満たすべき条件は＋や×のように順番に影響されない手続きであることだ

;; 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; prime
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ 1 test-divisor)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))


;; 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; 2.41
(define (equal-sum-of-unique-trio n s)
  (filter (lambda (l) (= s (+ (car l) (cadr l) (caddr l))))
          (flatmap
           (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n))))

;; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions)
  (define (iter lis)
    (cond ((null? lis) #t)
          ((= (car (car positions)) (car (car lis))) #f)
          ((= (- (cadar positions) (cadar lis)) (abs (- (car (car positions)) (car (car lis))))) #f)
          (else (iter (cdr lis)))))
  (iter (cdr positions)))


;; 2.43
(define (l-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (eve-fibs n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

(define (fib n)
  (define (iter a b k)
    (if (= k 0)
        a
        (iter b (+ a b) (- k 1))))
  (iter 0 1 n))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 1 n)))))

(define (even-fibs n)
  (filter even?
          (map fib
               (enumerate-interval 1 n))))

(define (list-fib-squares n)
  (map square
       (map fib
            (enumerate-interval 0 n))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              nil
              (map salary
                   (filter programmer? records))))

(define (map op sequence)
  (accumulate (lambda (x y) (cons (op x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms))) 0 coefficient-sequence))

(define (count-leaves tree)
  (accumulate +
              0
              (map (lambda (x) (if (not (pair? x))
                                   1
                                   (count-leaves x)))
                   tree)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (map op . args)
  (define (one-map op args)
    (if (null? args)
        nil
        (cons (op (car args))
              (one-map op (cdr args)))))
  (define (some-map op args)
    (if (null? (car args))
        nil
        (cons (apply op (one-map car args))
              (some-map op (one-map cdr args)))))
  (if (null? (cdr args))
      (one-map op (car args))
      (some-map op args)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector x cols)) m)))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor 2 n))

(define (find-divisor k n)
  (cond ((= k n) n)
        ((divides? k n) k)
        (else (find-divisor (+ 1 k) n))))

(define (divides? k n)
  (= (remainder n k) 0))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= item x)))
          sequence))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (sum-unique-trio-equal-s n s)
  (filter (lambda (x) (= s (+ (car x) (cadr x) (caddr x))))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

;; このpositionは((5 3) (5 2) (3 1))といったリストが入っている．
;; carの対が今新しく追加した対．これとcdrでfilteringする．
(define (safe? k positions)
  (define (iter lis)
    (let ((row caar) ;;行はrow
          (col cadar)) ;;列はcol
      (cond ((null? lis) #t)
          ((= (row positions) (row lis)) #f)
          ((= (abs (- (row positions) (row lis)))
              (- (col positions) (col lis))) #f)
          (else (iter (cdr lis))))))
  (iter (cdr positions)))

(define (wave2 (beside wave (flip-vert wave))))

(define (wave4 (below wave2 wave2)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert))))
    (below painter2 painter2)))

(define (wave4 (flipped-pairs wave)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner-split (painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (corner-split painter n)
  (if (= n 0)
      paitner
      (let* ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner-split (painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

(define (square-limit painter n)
  (let ((quarter (corner-split paitnter n)))
    (let ((half (beside (flip-horiz quater) quater)))
      (below (flip-vert half) half))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter ) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  idnetity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split first second) painter (- n 1))))
          (first painter ((second smaller smaller)))))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; 2.46
(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;; 2.47
;; listでmake
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;; consでmake
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; 2.48
(define (make-segment v1 v2)
  (make-vect v1 v2))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;; 2.49
;; a
(define (outline-painter frame)
  (let* ((v00 (make-vect 0 0))
         (v01 (make-vect 0 1))
         (v10 (make-vect 1 0))
         (v11 (make-vect 1 1)))
    ((segments->painter (list (make-segment v00 v01)
                              (make-segment v00 v10)
                              (make-segment v10 v01)
                              (make-segment v01 v10)))
     frame)))

;; b
(define (x-painter frame)
  (let* ((v00 (make-vect 0 0))
         (v01 (make-vect 0 1))
         (v11 (make-vect 1 1))
         (v10 (make-vect 1 0)))
    ((segments->painter (list (make-segment v00 v11)
                              (make-segment v01 v10)))
     frame)))

;; c
(define (Rhombus-painter frame)
  (let* ((va (make-vect 0.5 0))
         (vb (make-vect 1 0.5))
         (vc (make-vect 0.5 1))
         (vd (make-vect 0 0.5)))
    ((segments->painter (list (make-segment va vb)
                              (make-segment vb vc)
                              (make-segment vc vd)
                              (make-segment vd va)))
     frame)))

;; d
;;  パス

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 1.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 paitner)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; 2.51
(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-bottom (transform-painter painter1
                                          (make-vect 0.0 0.0)
                                          (make-vect 1.0 0.0)
                                          split-point))
         (paint-upper (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.5)
                                         0.0 1.0)))
    (lambda (frame)
      (paint-bottom)
      (paint-upper))))

(define (below painter1 painter2)
  (lambda (frame)
    (rotate270 (beside (rotate90 painter2)
                       (rotate90 painter1)))))

;; 2.52
;; a
;; 変更前のwaveはコピペ
(define wave-painter
  (segments->painter
   (list (make-segment (make-vect 0.5 0.6) (make-vect 0.2 0.25)) ;;笑った感じになるかはわからないけど
         (make-segment (make-vect 0.5 0.4) (make-vect 0.2 0.25)) ;;ここに二辺追加した
         (make-segment (make-vect 0.000 0.645) (make-vect 0.154 0.411))
         (make-segment (make-vect 0.154 0.411) (make-vect 0.302 0.588))
         (make-segment (make-vect 0.302 0.588) (make-vect 0.354 0.497))
         (make-segment (make-vect 0.354 0.497) (make-vect 0.245 0.000))
         (make-segment (make-vect 0.419 0.000) (make-vect 0.497 0.171))
         (make-segment (make-vect 0.497 0.171) (make-vect 0.575 0.000))
         (make-segment (make-vect 0.748 0.000) (make-vect 0.605 0.462))
         (make-segment (make-vect 0.605 0.462) (make-vect 1.000 0.142))
         (make-segment (make-vect 1.000 0.354) (make-vect 0.748 0.657))
         (make-segment (make-vect 0.748 0.657) (make-vect 0.582 0.657))
         (make-segment (make-vect 0.582 0.657) (make-vect 0.640 0.857))
         (make-segment (make-vect 0.640 0.857) (make-vect 0.575 1.000))
         (make-segment (make-vect 0.419 1.000) (make-vect 0.354 0.857))
         (make-segment (make-vect 0.354 0.857) (make-vect 0.411 0.657))
         (make-segment (make-vect 0.411 0.657) (make-vect 0.285 0.657))
         (make-segment (make-vect 0.285 0.657) (make-vect 0.154 0.605))
         (make-segment (make-vect 0.154 0.605) (make-vect 0.000 0.857)))))

;; b
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below painter right corner)))))

;; c
(define (corner-split painter n)
  (if (= n 0)
      (flip-horiz painter)
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner-split (painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

