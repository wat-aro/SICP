(define (smallest-divisor n)
  (if (= n 1)
      0
      (find-divisor n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ (if (= test-divisor 2) 1 2) test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ 2 n)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else (remainder (* b (expmod b (- e 1) m))
                         m))))


(define (1+ a) (+ 1 a))

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (sum cube a 1+ b))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1 (* x (+ x 2))))
       a
       (lambda (x) (+ 4 x))
       b))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral-simpson f a b n)
  (define h
    (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (next i)
    ()))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next i) (+ i 1))
  (define (term i)
    (* (cond ((or (= i 0) (= i n)) 1)
             ((even? i) 2)
             (else 4))
       (y i)))
  (* (/ h 3.0)
     (sum term
          a
          next
          n)))

(define (integral-with-simpson2 f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next i) (+ i 1))
  (define (term i)
    (* (if (even? i) 2 4)
       (y i)))
  (* (/ h 3.0)
     (+ (y 0)
        (y n)
        (sum term
             1
             next
             (- n 1)))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31
;; 再帰的プロセスでproduct
(define (product term a next b)
  (if (> a b)
      0
      (* (term a)
         (product term (next a) next b))))

;; 反復的プロセスでproduct
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 1+ n))

(define (pi-product n)
  (define (term i)
    (if (odd? i)
        (/ (+ i 1) (+ i 2))
        (/ (+ i 2) (+ i 1))))
    (product term 1 1+ n))

(define (pi n)
  (* 4 (pi-product n)))

;; 1.32
;; 再帰的プロセスで
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;; 反復的プロセスで
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum a b)
  (accumulate + 0 (lambda (a) a) a 1+ b))

;; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate filter combiner null-value
                                                   term (next a) next b)))
        (else (filterd-accumulate filter combiner null-value term (next a) next b))))

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

(define (sum-prime-square a b)
  (filtered-accumulate prime? + 0 (lambda (a) (square a)) a 1+ b))

(define (product-disjoint-n n)
  (define (disjoint-n? a)
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))))
    (= (gcd a n) 1))
  (filtered-accumulate disjoint-n? * 1 (lambda (a) a) 1 1+ n))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (f g)
  (g 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define (average a b)
  (/ (+ a b) 2))

(define (reduce f a x y b fx fy)
  (cond ((close-enough? a b) x)
        ((> fx fy)
         (let ((new (x-point a y)))
           (reduce f a new x y (f new) fx)))
        (else
         (let ((new (y-point x b)))
           (reduce f x y new b fy (f new))))))

(define (x-point a b)
  (+ a (* golden-ratio-squared (- b a))))

(define (y-point a b)
  (+ a (* golden-ratio (- b a))))

(define golden-ratio
  (/ (- (sqrt 5) 1) 2))

(define golden-ratio-squared (square golden-ratio))

(define (golden f a b)
  (let ((x (x-point a b))
        (y (y-point a b)))
    (reduce f a x y b (f x) (f y))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; 1.35
(define golden-ratio
  (fixed-point (lambda (x) (+ 1 (/ x)))
               1.0))

;; 1.36
(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (my-exp)
  (fixed-point2 (lambda (x) (/ (log 1000) (log x)))
                2.0))

(define (my-exp2)
  (fixed-point2 (lambda (x) (average x (/ (log 1000) (log x))))
                2.0))

;; 1.37
;; 再帰的プロセス
(define (cont-frac n d k)
  (define (recur i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))


;; 反復的プロセス
(define (cont-frac n d k)
  (define (iter i res)
    (if (= i 0)
        res
        (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter (- k 1) (/ (n k) (d k))))


(define (iter-a-to-b f a b)
  (newline)
  (display a)
  (display " -> ")
  (if (= a b)
      (f a)
      (and (display (f a)) (iter-a-to-b f (+ a 1) b))))

(iter-a-to-b (lambda (k)
               (cont-frac (lambda (i) 1.0)
                          (lambda (i) 1.0)
                          k))
             1 20)

(define (e-2 k)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) (if (= (modulo i 3) 2)
                             (* 2 (+ 1 (quotient i 3)))
                             1.0))
             k))

(iter-a-to-b (lambda (k)
               (+ 2
                  (e-2 k)))
             1
             20)

;; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (- (square x))))
             (lambda (i) (- (* 2.0 i) 1.0))
             k))

(define (deriv f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

(define (cube x) (* x x x))

(define (newton f guess)
  (if (good-enough? guess f)
      guess
      (newton f (improve guess f))))

(define (improve guess f)
  (- guess (f (f guess)
              ((deriv f .001) guess))))

(define (good-enough? guess f)
  (< (abs (f guess)) .001))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x) c)))

(define (inc n)
  (+ 1 n))
;; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

(define D double)
(define DD (D D))

(((D (D D)) inc) 5)
(((D DD) inc) 5)
((DD (DD inc)) 5)
((DD (D (D inc))) 5)
((D (D (D (D inc)))) 5)
((D (D (D (lambda (x) (+ 2 x))))) 5)
((D (D (lambda (x) (+ 4 x)))) 5)
((D (lambda (x) (+ 8 x))) 5)
((lambda (x) (+ 16 x)) 5)
(+ 16 5)
21

;; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;; 1.43
(define (repeated f n)
  (define (iter fn count)
    (if (= count n)
        fn
        (iter (compose f fn) (+ count 1))))
  (iter f 1))

;; 1.44
(define (smooth f)
  (let ((dx 0.0001))
    (lambda (x)
      (/ (+ (f (- x dx))
            (f x)
            (f (+ x dx)))
         3))))

(define (n-fold-smooth f n)
  (lambda (x)
    ((repeated smooth n) x)))


;; 1.45
;; 2^x, n回平均緩和
(define (test x n)
  (fixed-point ((repeated average-damp n) (lambda (y)
                                            (/ x (expt y (- x 1)))))
               1.0))

;; x^n k回平均緩和
(define (test x n k)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (lambda (z) ((repeated average-damp k) z))
                            1.0))

(define (nth-root x n)
  (let ((k (floor (sqrt x))))
    (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                              (lambda (z) ((repeated average-damp k) z))
                              1.0)))

(define (nth-root x n)
  (let ((k (floor (log n 2))))
    (fixed-point ((repeated average-damp k)
                  (lambda (y)
                    (/ x (expt y
                               (- x 1)))))
                 1.0)))

;; 1.46
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
          next
          ((iterative-improve good-enough? improve) next)))))

(define (iterative-improve enough? improve)
  (lambda (guess)
    (define (iter guess)
      (if (enough? guess)
          (improve guess)
          (iter (improve guess))))
    (iter guess)))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- 1 (/ guess (improve guess)))) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define (improve guess)
    (f guess))
  (define (close-enough? guess)
    (< (abs (- guess (improve guess))) 0.00001))
  ((iterative-improve close-enough? improve) first-guess))

