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

(define (mul-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; 2.1
(define (make-rat n d)
  (let* ((g (gcd n d))
         (n1 (/ n g))
         (d1 (/ d g)))
    (if (< d1 0)
        (cons (* -1 n1) (* -1 d1))
        (cons n1 d1))))


(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

(define (midpoint-segment seg)
  (let ((s-seg (start-segment seg))
        (e-seg (end-segment seg)))
    (make-point (/ (+ (x-point s-seg)
                      (x-point e-seg))
                   2)
                (/ (+ (y-point s-seg)
                      (y-point e-seg))
                   2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


;; 2.3
;; 長方形を幅と高さで定義
(define (make-rectangle width height)
  (cons width height))

(define (perimeter-rect rect)
  (+ (* 2 (width-rect rect))
     (* 2 (height-rect rect))))

(define (area-rect rect)
  (* (height-rect rect)
     (width-rect rect)))

(define (width-rect rect)
  (car rect))

(define (height-rect rect)
  (cdr rect))

;; 長方形を対角の二点によって定義
(define (make-rect p1 p2)
  (cons p1 p2))

(define (point-1 rect)
  (car rect))

(define (point-2 rect)
  (cdr rect))

(define (height-rect rect)
  (abs (- (y-point (point-1 rect))
          (y-point (point-2 rect)))))

(define (width-rect rect)
  (abs (- (x-point (point-1 rect))
          (x-point (point-2 rect)))))

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))

;; 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons x y))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x

;; cdr
(define (cdr z)
  (lambda (p q) q))

;; 2.5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (car c)
  (define (iter n count)
    (if (< 0 (remainder n 2))
        count
        (iter (/ n 2) (+ count 1))))
  (iter c 0))

(define (cdr c)
  (define (iter n count)
    (if (< 0 (remainder n 3))
        count
        (iter (/ n 3) (+ count 1))))
  (iter c 0))


(define (car c)
  (define (iter n)
    (if (= 0 (modulo n 3))
        (iter (/ n 3))
        (log n 2)))
  (iter c))

(define (cdr c)
  (define (iter n)
    (if (= 0 (modulo n 2))
        (iter (/ n 2))
        (log n 3)))
  (iter c))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 zero)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))


(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(+ one two)
(lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f x))) f) (((lambda (f) (lambda (x) (f (f x)))) f) x))))
(lambda (f) (lambda (x) ((lambda (x) (f x)) ((lambda (x) (f (f x))) x))))
(lambda (f) (lambda (x) ((lambda (x) (f x)) (f (f x)))))
(lambda (f) (lambda (x) (f (f (f x)))))


(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; 2.7
(define (make-interval a b) (cons a b))

;; 2.8
(define (upper-bound x)
  (cdr x))

(define (lower-bound x)
  (car x))

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (upper-bound y) (lower-bound x))))

;; 2.9


;; 2.10
;; y が0をまたがる区間の時はエラーを返す
(define (div-interval x y)
  (if (> 0 (* (lower-bound y) (upper-bound y)))
      (error "error")
      (mul-interval x
                    (make-interval (/ (upper-bound y))
                                   (/ (lower-bound y))))))

;; 2.11
(define (div-interval x y)
  (let ((lowx (lower-bound x))
        (upx (upper-bound x))
        (lowy (lower-bound y))
        (upy (upper-bound y)))
    (cond ((> lowx 0)        ;;xは正
           (cond ((> lowy 0) ;;yは正
                  (make-interval (* lowx lowy)
                                 (* upx upy)))
                 ((< upy 0) ;;yは負
                  (make-interval (* upx upy)
                                 (* lowx lowy)))
                 (else ;;yは０を跨ぐ
                  (make-interval (* upx lowy)
                                 (* upx upy)))))
          ((< upx 0) ;;xは負
           (cond ((> lowy 0) ;;yは正
                  (make-interval (* upx upy)
                                 (* lowx lowy)))
                 ((< upy 0) ;;yは負
                  (make-interval (* lowx lowy)
                                 (* upx upy)))
                 (else ;;yは０を跨ぐ
                  (maek-interval (* upx upy)
                                 (* upx lowy)))))
          (else ;;xは０を跨ぐ
           (cond ((> lowy 0) ;;yは正
                  (make-interval (* lowx upy)
                                 (* upx upy)))
                 ((< upy 0) ;;yは負
                  (make-interval (* upx upy)
                                 (* lowx upy)))
                 (else ;;yは０を跨ぐ
                  (make-interval (if (< (* lowx loxy) (* upx upy))
                                     (* upx upy)
                                     (* lowx lowy))
                                 (if (< (* lowx upy) (* upx lowy))
                                     (* lowx upy)
                                     (* upx lowy)))))))))


(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

