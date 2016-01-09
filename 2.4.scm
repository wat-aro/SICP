;; 2.4

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (img-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan (y x))))

(define (make-from-mag-ang r a) (cons r a))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar
              (cons r a)))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMSG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contens z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (apply-genelic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args)
                 (error
                  "No method for these types -- APPLY-GENERIC"
                  (list op type-tags)))))))


;; 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else (get 'deriv (operator exp)) (operands exp) var)))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;; a
;; 元のプログラムと違うのはelseの行．
;; operatorの型に合わせたderivが呼ばれ残りの要素を処理する．
;; number?とvariable?はリストでないので型を持たないため，データ主導の振り分けに吸収できない．


;; b
(define (install-deriv-sum-package)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (make-sum a1 a2)
    (cond ((= a1 0) a2)
          ((= a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (addend x) (cadr x))

  (define (augend x) (caddr x)
    (if (null? (cdddr x))
        (caddr x)
        (cons '+ (cddr x))))

  (put 'deriv '+ deriv-sum)
  (put 'make '+ make-sum)
  'done)

(define (install-deriv-product-package)
  (define (deriv-product exp var)
    ((get 'make-sum '+)
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (multiplicand exp)
                   (deriv (multiplier exp) var))))

  (define (make-product m1 m2)
    (cond ((or (= m1 0) (= m2 0)) 0)
          ((= m1 1) m2)
          ((= m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (multiplier x) (cadr x))

  (define (multiplicand x)
    (if (null? (cdddr x))
        (caddr x)
        (cons '* (cddr x))))

  (put 'deriv '* deriv-product)
  (put 'make '* make-product)

  'done)

(define (install-exponent-package)
  (define (deriv-exponent exp var)
    (let ((make-product (get make '*)))
      (make-product
       (make-product (exponent x)
                     (make-exponentiation (base x)
                                          (- (exponent x) 1)))
       (deriv (base x) var))))

  (define (exponent x) (cadr x))

  (define (base x) (caddr x))

  (define (make-exponent b e)
    (cond ((= e 0) 1)
          ((= e 1) b)
          ((= b 0) 0)
          (else (list '** b e))))

  (put 'deriv '** deriv-exponent)
  (put 'make '** make-exponent)
  'done)

;; d
;; それぞれの補助関数の中で
;; sum
(define (addend x) (car x))
(define (augend x)
  (if (null? (cddr x))
      (cdr x)
      (cons '+ (cdr x))))

;; product
(define (multiplier x) (car x))
(define (multiplicand x)
  (if (null? (cddr x))
      (cdr x)
      (cons '+ (cdr x))))

;; exponent
(define (base x) (car x))
(define (exponent x) (cdr x))



;; 2.74
;; a
;; 各事業所ごとに従業員ファイルを作っていると考え，person-fileのcar部に
;; 従業所を識別するコードを入れるようにする．
(define (get-record name  person-file)
  ((get 'get-record (division person-file)) name file))

(define (division file) (car file))

;; b
;; ここではrecordが(name salary age)となってると考える．
;; この事業所のrecordからsalaryを取り出すにはcadrを取れば良い
(define (get-salary name record)
  ((get 'get-salary (identifying person-file)) record))

;; c
(define (find-employee-record name division-list)
  (if (null? division-list)
      false
      (let ((serch (lambda (x) ((get 'get-record (division x)) name x))))
        (if (serch (car division-list))
            (serch (car division-list))
            (find-employee-record name (cdr division-list))))))

;; d
;; その新しく合併した会社の従業員レコードから情報を得るget-recordとget-salaryなどの
;; 必要な手続きを作りパッケージを作成し，本社の表にputすればよい．

;; メッセージパッシング
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))


;; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; 2.76
;; ;; 明示的ディスパッチによるジェネリック演算
;; データの型が追加されるたびに各演算にその型用の演算を追加していく．
;; 新しい演算が追加されたときはそれを追加するだけ．

;; ;; データ主導スタイル
;; データの型が追加されると，それらをパッケージを作ってputする．
;; 各型パッケージにその演算を追加する．

;; ;; メッセージパッシングスタイル
;; データが追加されたときは特になし
;; 新しい演算が追加されたときは，各データ型に演算を追加する．

;; データ新しい型がよく追加されるならメッセージパッシングスタイルが向いている．
;; 新しい演算が追加されるときには明示的ディスパッチが向いている．

