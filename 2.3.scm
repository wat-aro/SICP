(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; 2.53
;; (list 'a 'b 'c)
;; (a b c)

;; (list (list 'george))
;; ((george))

;; (cdr '((x1 x2) (y1 y2)))
;; ((y1 y2))

;; (cadr '((x1 x2) (y1 y2)))
;; (y1 y2)

;; (pair? (car '(a short list)))
;; #f

;; (memq 'red '((red shoes) (blue socks)))
;; #f

;; (memq 'red '(red shoes blue socks))
;; (red shoes blue socks)

;; 2.54
(define true #t)
(define false #f)

(define (equal? a b)
  (cond ((and (null? a) (null? b)) true)
        ((or (null? a) (null? b)) false)
        ((not (pair? a)) (eq? a b))
        (else (and (eq? (car a) (car b))
                   (equal? (cdr a) (cdr b))))))

(define (equal? a b)
  (or (and (not (pair? a))
           (not (pair? b))
           (eq? a b))
      (and (pair? a)
           (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))

;; 2.55
;; ''abracadabraは'abracadabraを返す．
;; (car ''abracadabra)はquoteを返す．
;; (cdr ''abracadabra)は(abracadabra)を返す．
;; つまり'abracadabraは(quote abracadabra)のことで，
;; ''abracadabraは'(quote abracadabra)のことである．
;; そのため(car ''abracadabra)はquoteを返す．

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))


(define (cons* a b . c)
  (if (null? c)
      (cons a b)
      (cons a (apply cons* (cons b c)))))

(define (append* a b . c)
  (if (null? c)
      (append a b)
      (append a (apply append* (cons b c)))))
;; 2.57
(define (addend s) (cadr s))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

;; 2.58
;; a
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base x)
  (car x))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e) (expt b e)))
        (else (list b '** e))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; 2.59
(define (unionset s t)
  (cond ((null? s) t)
        ((element-of-set? (car s) t)
         (unionset (cdr s) t))
        (else (cons (car s)
                    (unionset (cdr s) t)))))
;; 2.60
(define (adjoin-set x s)
  (cons x s))

(define (union-set s t)
  (append s t))

;; element-of-set?やintersection-setについてはsetの中身が増えることで比較回数が増えて効率は下がる．
;; adjoin-set union-setについては条件分岐がなくなるので効率がよくなる．


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
            (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1)
                                          (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; 2.61
(define (adjoin-set x s)
  (cond ((null? s) (list x))
        ((= x (car s)) s)
        ((< x (car s)) (cons x s))
        (else (cons (car s) (adjoin-set x (cdr s))))))

;; 同じ数字，またはxより大きい数字が出てきた時点で計算が終わるので順序付けられない表現に比べ半分のステップ数ですむ．

;; 2.62
(define (union-set s t)
  (cond ((null? s) t)
        ((null? t) s)
        ((= (car s) (car t))
         (cons (car s)
               (union-set (cdr s) (cdr t))))
        ((< (car s) (car t))
         (cons (car s)
               (union-set (cdr s) t)))
        ((< (car t) (car s))
         (cons (car t)
               (union-set s (cdr t))))))

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (/ (- n 1) 2))))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elts))))))))

;; 2.65
(define (union-tree s t)
  (list->tree
   (union-set (tree->list-2 s)
              (tree->list-2 t))))

(define (intersection-tree s t)
  (list->tree
   (intersection-set-local (tree->list-2 s)
                           (tree->list-2 t))))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;; 2.66
(define (lookup-tree given-key set-of-records)
  (cond (let ((key-record (key (car set-of-records))))
          ((null? set-of-records) false)
          ((= given-key key-record)
           (car set-of-records))
          ((< given-key key-record)
           (lookup-tree given-key (left-branch set-of-records)))
          ((< key-record given-key)
           (lookup-tree given-key (right-branch set-of-records))))))

;; 2.58b
;; partには'beforeか'afterが入り，symbolの位置でexpを前後に分ける．
(define (extract part symbol exp)
  (define (iter subexp remaining)
    (cond ((null? remaining) remaining)
          ((eq? (car remaining) symbol)
           (cond ((eq? part 'before) subexp)
                 ((eq? part 'after) (cdr remaining))
                 (else (error "Unclear, do you mean 'before or after?"))))
          (else
           (iter (append subexp (list (car remaining)))
                 (cdr remaining)))))
  (let ((result (iter nil exp)))
    (if (eq? (length result) 1)
        (car result)
        result)))

;; リストにシンボルが入っているかを問う述語
(define (contains? symbol lis)
  (cond ((or (null? lis) (not (pair? lis))) false)
        ((eq? symbol (car lis)) true)
        (else (contains? symbol (cdr lis)))))

;; sum
(define (sum? x)
  (contains? '+ x))

(define (addend s)
  (extract 'before '+ s))

(define (augend s)
  (extract 'after '+ s))

;; product
(define (product? x)
  (contains? '* x))

(define (multiplier p)
  (extract 'before '* p))

(define (multiplicand p)
  (extract 'after '* p))

;; exponentiation
(define (exponentiation? e)
  (contains? '** e))

(define (base e)
  (extract 'before '** e))

(define (exponent e)
  (extract 'after '** e))



;; 簡約

;; かっこから出す
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

;; 演算子で分けられたリストに分ける．
(define (split-by op polynome)
  (cond ((null? polynome) '())
        ((or (not (pair? polynome))
             (not (contains? op polynome))) ;;追加．これがないと最後の項がシングルトン以外の場合空リストになる．
         (list polynome))
        (else (append (list (extract 'before op polynome))
                      (split-by op (extract 'after op polynome))))))

(define (summands polynome)
  (split-by '+ polynome))

(define (factors polynome)
  (split-by '* polynome))

;; リストの要素の間にopを入れる
(define (infix op lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) lst)
        (else (append (list (car lst))
                      (cons op
                            (infix op (cdr lst)))))))

(define (infix-add s)
  (infix '+ s))

(define (infix-multiply p)
  (infix '* p))

;; リストの先頭のリストにだけopを適用する．
(define (apply-car op lst)
  (append (list (apply op (car lst)))
          (cadr lst)))

(define (apply-car+ s)
  (apply-car + s))

(define (apply-car* p)
  (apply-car * p))

;; (6)を6といった具合に数一つだけのリストをnumberにする
(define (release-singleton e)
  (if (= (length e) 1)
      (car e)
      e))

;; 数だけを先頭にあつめてリストにする
(define (group lst)
  (cons (filter number? lst)
        (list (filter (lambda (n) (not (number? n)))
                      lst))))

;; リストの先頭を最後にもっていく．
(define (shift-left lst)
  (append (cdr lst) (list (car lst))))


;; まずfringeでかっこを外し，summandsを使い，＋の位置で分けたリストに変換する．
;; そのリストに対してmapで各要素にfactors,group,apply-car*,release-singletonの順に手続きを適用する．
;; つまり，*でわけたリストに変換し，数字のみのリストをcarにもってきて，それに*を適用し，要素の間に＊をいれ，シングルトンがあれば，それを数字にする．
;; これが全要素に完了した後に出来たリストに対して，group,apply-car+,shift-left,infix-add,fringeを順に適用する．
;; 先頭に数字のみでできたリストを作り，それらを足し，リストの最後に移す．このリストの要素の間に＋を挿入し，かっこを取り払う．
(define (simplify polynome)
  ((compose fringe
            infix-add
            shift-left
            apply-car+
            group)
   (map (compose release-singleton
                 infix-multiply
                 apply-car*
                 group
                 factors)
        (summands (fringe polynome)))))


;; 2.3.4
;; Huffman木

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; bitが0なら左，1なら右の枝をたどっていき，leafにたどり着くと，そのシンボルをconsして次にいく．
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

;; bitが左ならleft-branch,bitが右ならright-branchを選択
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; weightで昇順に順序づけられた集合に要素を追加する
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; pairsからweightの昇順にleafの集合を作る．
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol msg tree)
  (if (leaf? tree)
      '()
      (cond
       ((memq msg (symbols (left-branch tree)))
        (cons 0
              (encode-symbol msg (left-branch tree))))
       ((memq msg (symbols (right-branch tree)))
        (cons 1
              (encode-symbol msg (right-branch tree))))
       (else (error msg "is not Found")))))

;; 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; pairsは昇順に並んでいるので先頭の2要素をmake-code-pairsする．
;; それを(cddr pairs)にadjoin-setすればまた昇順に並んだpairsができるのでそれを繰り返す．
(define (successive-merge pairs)
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge (adjoin-set (make-code-tree (car pairs)
                                                     (cadr pairs))
                                    (cddr pairs)))))

;; 2.70
(define q-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3)
                       (YIP 9) (WAH 1)))

(define q-tree
  (successive-merge (make-leaf-set q-pairs)))

(define message
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

;; 2.71
(define n5-pairs '((n1 1) (n2 2) (n3 4) (n4 8) (n5 16)))
(define n5-tree (successive-merge (make-leaf-set n5-pairs)))
