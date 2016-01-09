(define (apply-genelic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args)
                 (error
                  "No method for these types -- APPLY-GENERIC"
                  (list op type-tags)))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;;内部手続き
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ n g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; システムのほかの部分とのインターフェース
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; 直交座標と極座標パッケージから取り入れた手続き
  (define (make-from-real-imag x y)
    ((get 'make-from-real-mag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; 内部手続き
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z3)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; システムの他の部分へのインターフェース
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; 2.77
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)


;; magnitudeはcomplex型を知らないのでerrorを返す．
;; なので表にcomplex型を追加すれば動く．


(magnitude z)
;=>
(magnitude (complex ractangular 3 . 4))
;=>
(apply-generic magnitude (complex ractangular 3 . 4))
;=>
((get 'magnitude '(complex)) (ractangular 3 . 4))
;=>
(magnitude (ractangular 3 . 4))
;=>
(apply-generic magnitude (ractangular 3 . 4))
;=>
((get 'magnitude '(ractangular)) (3 . 4))
;=>
(magnitude (3 . 4))
;=>
(sqrt (+ (square 3) (square 4)))
;=>
5

;; 2.78
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? (car datum)) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? (car datum)) 'scheme-number)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

;; 2.79
;; scheme-numberパッケージに追加
(put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (tag (= x y))))

;; rationalパッケージに追加
(put 'equ? '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y))
                          (= (denom x) (denom y)))))
;; complexパッケージに追加
(put 'equ '(complex complex)
     (lambda (x y) (and (= (real-part x) (real-part y))
                        (= (imag-part x) (imag-part y)))))


(define (equ? x y) (apply-generic 'equ? x y))

;; 2.80
(define (=zero? x) (apply-generic '=zero? x y))

;; scheme-numberパッケージに追加
(put '=zero? '(scheme-number)
     (lambda (x) (= x 0)))

;; rationalパッケージに追加
(put '=zero? '(rational)
     (lambda (x) (= (numer x) 0)))

;; complexパッケージに追加
(put '=zero? '(complex)
     (lambda (x) (and (= 0 (real-part x))
                      (= 0 (imag-part x)))))

;; 複素数パッケージに含める
(define (add-complex-to-schemenum z x)
  (make-from-real-imag (+ (real-part z) x) (imag-part z)))
(put 'add '(complex scheme-number)
     (lambda (z x) (tag (add-complex-to-schemenum z x))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              scheme-number->complex)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc ;;false
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2 ;;true
                         (apply-generic op (t1->t2 a1) a2)) ;;complex->complex
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "Nomethod for these types"
                     (list op type-tags)))))))

;; 2.81
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

;; a
(define (exp x y) (apply-generic 'exp x y))

;; scheme-numberパッケージに追加する
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y))))

;; 引数に二つの複素数を持ってexpを呼び出すと，
;; procがfalseになり，complexからcomplexへの変換を無限ループする

;; b
;; 無限ループに陥るのでLouisはまちがっている

;; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (eq? (car type-tags) (cadr type-tags))) ;;同じtype-tagならエラーになる
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "Nomethod for these types"
                     (list op type-tags)))))))

;; 2.82
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (define (try-coercion args tags)
      (if (null? tags)
          (error "Nomethod for these types"
                 (list op type-tags))
          (let ((new-args (map (lambda (x) (get-coercion (car tags)
                                                         (type-tag x)))
                               args)))
            (let ((new-type-tags (map type-tag new-args)))
              (let ((peroc (get op (new-type-tags))))
                (if proc
                    (apply proc (contents new-args))
                    (try-coercion new-args (cdr tags))))))))
    (let ((pero (get op types)))
      (if proc
          (apply proc (map contents args))
          (try-coercion args typep-tags)))))

;; 2.83
(define (raise x) (apply-generic 'raise x))

;; scheme-numberパッケージで
(define (scheme-number->rational n)
  (make-rational n 1))
(put 'raise 'scheme-number scheme-number->rational)

;; rationalパッケージで
(define (rational->real x)
  (/ (* 1.0 (numer x)) (denom x)))
(put 'raise 'rational rational->real)

;; real-numberパッケージで
(define (real->complex)
  (make-complex-from-real-imag x 0))
(put 'raise 'real-number real->complex)

;; 2.84
;; 同じタイプか調べる述語
(define (same-type? a b)
  (eq? (type-tag a) (type-tag b)))
;; aよりもbのほうが階層が高いか調べる述語
;; 両方をraiseしながらcomplexに先になったほうが階層が高い
(define (type-< a b)
  (let ((tower '(complex real rational scheme-number)))
    (cond ((same-type? a b) false)
          ((eq? (type-tag a) (car tower)) true)
          ((eq? (type-tag b) (car tower)) false)
          (else (type-< ((get 'raise (type-tag a)) a)
                        ((get 'raise (type-tag b)) b))))))
;; リストの中でもっとも高い階層の型を調べる
(define (highest-type lst)
  (let iter ((result (car lst))
             (rest (cdr lst)))
    (cond ((null? rest) result)
          ((type-< result (car rest))
           (iter (car rest) (cdr rest)))
          (else
           (iter result (cdr rest))))))
;; リストの要素すべてを最も階層の高い型highまでraiseする
(define (same-highest-type high lst)
  (let iter ((result '())
             (rest (cdr lst)))
    (cond ((null? rest) result)
          ((eq? high (car rest))
           (iter (append result (list (car rest)))
                 (cdr rest)))
          (else
           (iter result
                 (cons ((get 'raise (type-tag (car rest))) (car rest))
                       (cdr rest)))))))



(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))
        (tower '(complex real rational scheme-number)))
    ;; 同じタイプか調べる述語
    (define (same-type? a b)
      (eq? (type-tag a) (type-tag b)))
    ;; aよりもbのほうが階層が高いか調べる述語
    ;; 両方をraiseしながらcomplexに先になったほうが階層が高い
    (define (type-< a b)
        (cond ((same-type? a b) false)
              ((eq? (type-tag a) (car tower)) true)
              ((eq? (type-tag b) (car tower)) false)
              (else (type-< ((get 'raise (type-tag a)) a)
                            ((get 'raise (type-tag b)) b)))))
    ;; リストの中でもっとも高い階層の型を調べる
    (define (highest-type lst)
      (let iter ((result (car lst))
                 (rest (cdr lst)))
        (cond ((null? rest) result)
              ((type-< result (car rest))
               (iter (car rest) (cdr rest)))
              (else
               (iter result (cdr rest))))))
    ;; リストの要素すべてを最も階層の高い型highまでraiseする
    (define (same-highest-type high lst)
      (map (lambda (x) (let iter ((target x))
                         (if (eq? high target)
                             target
                             (iter ((get 'raise (type-tag target))
                                    target)))))
           lst))
    (let ((proc (get op types)))
      (if proc
          (apply proc (map contents args))
          (let ((new-args (same-highest-type (highest-type args)
                                             args)))
            (let ((proc (get op (type-tag (car new-args)))))
              (if proc
                  (apply proc (map contents new-args))
                  (error "Nomethod for these types"
                         (list op type-tags)))))))))

(define (my-max lst)
  (let iter ((result (car lst))
             (rest (cdr lst)))
    (cond ((null? rest) result)
          ((< result (car rest))
           (iter (car rest) (cdr rest)))
          (else (iter result (cdr rest))))))

(define (my-< a b)
  (cond ((= a b) false)
        ((= a 0) true)
        ((= b 0) false)
        (else (my-< (- a 1) (- b 1)))))

;; 2.85
(define (install-project-packege)
  (define (project x) (apply-generic 'project x))
  (put 'project 'complex (lambda (x)
                           (make-real (real-part x))))
  (put 'project 'real (lambda (x)
                        (let ((rational (inexact->exact x)))
                          (make-rational (numerator rational)
                                         (denominator rational)))))
  (put 'project 'rational (lambda (x)
                            (make-scheme-number (round (/ (numer x)
                                                          (denom x))))))
  'done)

(define (drop x)
  (let ((projected ((get 'project (type-tag x)) x)))
    (let ((raised ((get 'raise (type-tag projected)) projected)))
      (if (equ? x raised)
        (drop projected)
        x))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))
        (tower '(complex real rational scheme-number)))
    (define (same-type? a b)
      (eq? (type-tag a) (type-tag b)))
    (define (type-< a b)
        (cond ((same-type? a b) false)
              ((eq? (type-tag a) (car tower)) true)
              ((eq? (type-tag b) (car tower)) false)
              (else (type-< ((get 'raise (type-tag a)) a)
                            ((get 'raise (type-tag b)) b)))))
    (define (highest-type lst)
      (let iter ((result (car lst))
                 (rest (cdr lst)))
        (cond ((null? rest) result)
              ((type-< result (car rest))
               (iter (car rest) (cdr rest)))
              (else
               (iter result (cdr rest))))))
    (define (same-highest-type high lst)
      (map (lambda (x) (let iter ((target x))
                         (if (eq? high target)
                             target
                             (iter ((get 'raise (type-tag target))
                                    target)))))
           lst))
    (let ((proc (get op types)))
      (if proc
          (drop (apply proc (map contents args))) ;;drop
          (let ((new-args (same-highest-type (highest-type args)
                                             args)))
            (let ((proc (get op (type-tag (car new-args)))))
              (if proc
                  (dorp (apply proc (map contents new-args))) ;;drop
                  (error "Nomethod for these types"
                         (list op type-tags)))))))))

;; 2.86
(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (atangent x) (apply-generic 'atangent x))

;; scheme-number
(put 'square '(scheme-number) (lambda (x) (* x x)))
(put 'square-root '(scheme-number) (lambda (x) (sqrt x)))
(put 'sine '(scheme-number) (lambda (x) (sin x)))
(put 'cosine '(scheme-number) (lambda (x) (cos x)))
(put 'atangent '(scheme-number) (lambda (x) (atan x)))

;; rational
(put 'square '(rational) (lambda (x) (make-rat (square (numer x))
                                               (square (denom x)))))
(put 'square-root '(rational) (lambda (x) (make-real (sqrt (project x)))))
(put 'sine 'rational (lambda (x) (make-real (sin (project x))))) ;;real
(put 'cosine 'rational (lambda (x) (make-real (cos (project x))))) ;;real
(put 'atangent 'rational (lambda (x) (make-real (atan (project x))))) ;;real

;; real
(put 'square '(real) (lambda (x) (square x)))
(put 'square-root '(real) (lambda (x) (sqrt x)))
(put 'sine '(real) (lambda (x) (sin x)))
(put 'cosine '(real) (lambda (x) (cos x)))
(put 'atangent '(real) (lambda (x) (atan x)))

;; complex
(put 'square '(complex)
     (lambda (x) (make-complex-from-real-imag (+ (square (real-part x))
                                                 (square (imag)))
                                              (* 2 (real-part x) (imag-part x)))))
(put 'square-root '(complex)
     (lambda (x) (make-complex-from-mag-ang (sqrt (magnitude x))
                                            (/ (angle x) 2))))
(put 'sine '(complex)
     (lambda (x) (make-complex-from-real-imag (sin (real-part x))
                                              (sin (imag-part x)))))
(put 'cosine '(complex)
     (lambda (x) (make-complex-from-real-imag (cos (real-part x))
                                              (cos (imag-part x)))))
(put 'atangent '(complex)
     (lambda (x) (make-complex-from-real-imag (atan (real-part x))
                                              (atan (imag-part x)))))

;;2.5.3
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (install-polynomial-package)
  ;; 内部手続き
  ;; 多項式型の表現
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2) (eq? v1 v2))
  (define (variable? x) (symbol? x))

  ;; 項と項リストの表現
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  ;; システムの他の部分とのインターフェース
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var term))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-term (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-term L1 (rest-term L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-term (rest-term L1)
                             (rest-term L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t2) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (the-empty-termlist) '())
(define (first-term term-list) (car termlist))
(define (rest-term term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-poly-nomial var terms)
  ((get 'make 'polynomial) var terms))

;; 2.87
;; polynominal-package
(define (=zero? p)
  (= 0 (coeff p)))

(put '=zero? '(polynominal)
     (lambda (p) (=zero? p)))


;; 2.88
(define (negative x) (apply-generic 'negative x))

;; scheme-number
(define (negative-integer x) (- x))
(put 'negative '(scheme-number) (lambda (x) (negative-integer x)))

;; rational
(define (negative-rational x)
  (make-rational (negative (numer x))
                 (denom x)))
(put 'negative '(rational) (lambda (x) (negative-raitonal x)))

;; real
(define (negative-real x) (make-real (- x)))
(put 'negative '(real) (lambda (x) (negative-real x)))

;; complex
(put 'negative '(complex) (lambda (x) (negative x)))

;; rect-angler
(define (negative-rectangler x)
  (make-complex-from-mag-ang (magnitude x)
                             (+ 180 (angle x))))
(put 'negative '(rectangler) (lambda (x) (negative-rectangler x)) )

;; real-imag
(define (negative-polar x)
  (make-complex-from-real-imag (negative (real-part x))
                               (negative (imag-part x))))
(put 'negative '(polar) (lambda (x) (negative-polar x)))


;; polynomial
(define (negative-term p)
  (mul-term (make-term 0 -1) p))

(define (sub-terms L1 L2)
  (cond ((empty-termlist? L2) L1)
        ((empty-termlist? L1)
         (negative-term L2))
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (sub-term (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   (negative-term L2)
                   (sub-term L1 (rest-term L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (sub (coeff t1) (coeff t2)))
                   (sub-term (rest-term L1)
                             (rest-term L2)))))))))

(put 'negative '(polynomil) (lambda (x) (negative-term x)))
(put 'sub '(polynomiial (lambda (x) (sub-terms L1 L2))))

;; 2.89
;; 濃い多項式に適している実装
(define (make-polynomial valiable term-list)
  (cons valiable term-list))

(define (valiable p)
  (car p))

(define (term-list p)
  (cdr p))

(define (valiable? v)
  (symbol? v))

(define (same-valiable? v1 v2)
  (and (valiable? v1) (valiable? v2) (eq? v1 v2)))

(define (=zero-term? L)
  (let ((L1 (term-list L)))
    (or (empty-termlist? L1)
        (and (=zero? (coeff (first L1)))
             (=zero-term? (rest-terms L1))))))

(define (adjoin-term term term-list) (cons term term-list))

(define (empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (cons coeff (iota order 0 0)))
(define (order term) (length (rest-terms term)))
(define (coeff term) (first-term term))

(define (negative-terms L)
  (if (empty-termlist? L)
      empty-termlist
      (addjoin-term (negative (first term))
                    (negative-terms (rest-terms L)))))

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2))
               (o1 (order L1))
               (o2 (order L2)))
           ((> o1 o2)
            (adjoin-term t1 (add-terms (rest-terms L1) L2)))
           ((< o1 o2)
            (adjoin-term t2 (add-terms L1 (rest-terms L2))))
           (else
            (addjoin-term (add t1 t2)
                          (add-terms (rest-terms L1) (rest-terms L2))))))))

(define (sub-terms L1 L2)
  (cond ((empty-termlist? L2) L1)
        ((empty-termlist? L1) (negative-terms L1))
        (else
         (let ((t1 (first-term L1))
               (t2 (first-term L2))
               (o1 (order L1))
               (o2 (order L2)))
           ((> o1 o2)
            (adjoin-term t1
                         (sub-terms (rest-terms L1) L2)))
           ((< o1 o2)
            (adjoin-term (negative t2)
                         (sub-terms L1 (rest-terms L2))))
           (else
            (adjoin-term (sub t1 t2)
                         (sub-terms (rest-terms L1) (rest-terms L2))))))))

(define (mul-terms L1 L2)
  (cond ((empty-termlist? L1) (the-empty-termlist))
        ((empty-termlist? L2) (the-empty-termlist))
        (else
         (add-terms
          (mul-term-by-all-terms
           (make-term (first-term L1)
                      (iota (length (rest-terms L1) 0 0)))
           L2)
          (mul-terms (rest-terms L1) L2)))))

(define (mul-term-by-all-terms t L)
  (if (empty-termlist L)
      (rest-terms t)
      (add-join-term (mul (first-term t) (first-term L))
                     (mul-term-by-all-terms t (rest-terms L)))))

;; 2.90
;; パス

;; 2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms (sub-terms L1
                                           (mul-terms L2
                                                      (make-term new-o
                                                                 new-c)))
                                L2)))
                (list (add-terms (list (make-term new-o new-c))
                                 (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div (term-list p1)
                      (term-list p2)))
      (error "Polys not in same var -- DIV POLY"
             (list p1 p2))))
