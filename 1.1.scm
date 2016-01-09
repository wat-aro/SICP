(define true #t)
(define false #f)

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;; 1.3
(define (sum-of-squares-learge2 a b c)
  (cond ((<= a b c) (sum-of-squares b c))
        ((>= a b c) (sum-of-squares a b))
        (else (sum-of-squares a c))))

;;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; bが０より大きい時は(+ a b)が評価され，それ以外は(- a b)が評価される

;; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))
;; 作用的順序を使う解釈系ではまず(test 0 (p))を
;; (if (= 0 0)
;;     0
;;     (p))
;; と評価し，(= 0 0)が#tなのでthenが評価され0が返る．
;; 正規順序の評価を使う解釈系では
;; (if (= 0 0)
;;     0
;;     (p))
;; と評価するところまでは同じだが，この後基本的演算子だけになるまで評価を繰り返す．
;; そのためまず(p)を評価し，その結果(p)が返り，それをまた評価しようとするので
;; 演算が終わらない．

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-good-enough? guess x)
  (< (abs (- 1.0 (/ guess (improve guess x)))) 0.001))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x)
                 x)))

(define (cube-improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- 1.0 (/ guess (cube-improve guess x)))) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improe guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0 x))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ count 1)
                   max-count)))
  (fact-iter 1 1 n))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
9

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; gosh> 1024

(A 2 4)
;; gosh> 65536

(A 3 3)
;; gosh> 65536

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 1.11
;; 再帰的プロセス
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; 反復的プロセス
(define (g n)
  ;; iterでは3 ≦ nの時のみの処理．n1 はf(n-1),n2はf(n-2),n3はf(n-3)
  (define (iter n1 n2 n3 count)
    (if (= count 0)
        n1
        (iter (+ n1 (* 2 n2) (* 3 n3))
              n1
              n2
              (- count 1))))
  (if (< n 3)
      n
      (iter 2 1 0 (- n 2)))) ;;3 ≦ nの時iter．n=1,2の時はn1,n2に渡してあるのでcountに渡す引数は(- n 2)

(define (pascals-triangle n k)
  (if (or (= k 1) (= n k))
      1
      (+ (pascals-triangle (- n 1) (- k 1))
         (pascals-triangle (- n 1) k))))

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (sine-count angle 0))

(define (sine-count angle count)
  (if (not (> (abs angle) 0.1))
      count
      (sine-count (/ angle 3) (+ count 1))))

(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (exp-iter b n 1))

(define (exp-iter b counter product)
  (if (= counter 0)
      product
      (exp-iter b
                (- counter 1)
                (* b product))))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b count product)
  (cond ((= count 0) product)
        ((even? count)
         (fast-expt-iter (square b)
                        (/ count 2)
                        product))
        (else (fast-expt-iter b (- count 1) (* b product)))))

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;; 末尾再帰にしてみた
(define (* a b)
  (*iter a b 0))

(define (*iter a b sum)
  (if (= b 0)
      sum
      (*iter a (- b 1) (+ a sum))))

(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))


(define (fast-* n m)
  (cond ((= m 0) 0)
        ((even? m) (double (fast-* n (halve m))))
        (else (+ n (fast-* n (- m 1))))))
;; fast-exptと同じように
(define (fast-* a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-*-iter (double a) (halve b) sum))
        (else (fast-*-iter a (- b 1) (+ a sum)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (halve count)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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

(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))


(use srfi-27)

(define (fermat-test n)
  (define a (+ 2 (random-integer (- n 2))))
  (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fermat-test n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime)
    (use srfi-11)
    (let-values (((a b) (sys-gettimeofday)))
      (+ (* a 1000000) b)))

(define (search-for-primes start end)
  (cond ((> start end) (newline))
        ((even? start) (search-for-primes (+ start 1) end))
        (else (timed-prime-test start)
              (search-for-primes (+ start 2) end))))

(define (search-for-primes start end)
  (define (iter start end)
    (cond ((> start end) (newline))
          ((prime? start) (and (timed-prime-test start)
                               (iter (+ 2 start) end)))
          (else (iter (+ 2 start) end))))
  (if (odd? start)
      (iter start end)
      (iter (+ 1 start) end)))

(define (next n)
  (if (= n 2)
      3
      (+ 2 n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (carmichael-test n)
  (define (try-loop a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (try-loop (+ a 1)))
          (else false)))
  (try-loop 1))

(define (f-expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (let ((tmp (remainder (square (f-expmod b (/ e 2) m)) m)))
           (if (= 1 tmp)
               0
               tmp)))
        (else (remainder (* b (f-expmod b (- e 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let* ((tmp (expmod base (/ exp 2) m))
                  (tmp2 (remainder (square tmp) m)))
             (if (and (< 1 tmp (- n 1))
                      (= 1 tmp2))
                 0
                 tmp2)))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (try-it a)
    (= 1 (expmod a (- n 1) n)))
  (try-it (+ 1 (random-integer (- n 1)))))

(define true #t)
(define false #f)

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

;; 1.3
(define (sum-of-squares-learge2 a b c)
  (cond ((<= a b c) (sum-of-squares b c))
        ((>= a b c) (sum-of-squares a b))
        (else (sum-of-squares a c))))

;;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; bが０より大きい時は(+ a b)が評価され，それ以外は(- a b)が評価される

;; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; (test 0 (p))
;; 作用的順序を使う解釈系ではまず(test 0 (p))を
;; (if (= 0 0)
;;     0
;;     (p))
;; と評価し，(= 0 0)が#tなのでthenが評価され0が返る．
;; 正規順序の評価を使う解釈系では
;; (if (= 0 0)
;;     0
;;     (p))
;; と評価するところまでは同じだが，この後基本的演算子だけになるまで評価を繰り返す．
;; そのためまず(p)を評価し，その結果(p)が返り，それをまた評価しようとするので
;; 演算が終わらない．

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-good-enough? guess x)
  (< (abs (- 1.0 (/ guess (improve guess x)))) 0.001))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (cube-improve guess x)
                 x)))

(define (cube-improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- 1.0 (/ guess (cube-improve guess x)))) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improe guess x) x)))
  (sqrt-iter 1.0 x))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0 x))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial n)
  (define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ count 1)
                   max-count)))
  (fact-iter 1 1 n))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
9

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;; gosh> 1024

(A 2 4)
;; gosh> 65536

(A 3 3)
;; gosh> 65536

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib n)
  (define (iter a b count)
    (if (= count 0)
        b
        (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; 1.11
;; 再帰的プロセス
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; 反復的プロセス
(define (g n)
  ;; iterでは3 ≦ nの時のみの処理．n1 はf(n-1),n2はf(n-2),n3はf(n-3)
  (define (iter n1 n2 n3 count)
    (if (= count 0)
        n1
        (iter (+ n1 (* 2 n2) (* 3 n3))
              n1
              n2
              (- count 1))))
  (if (< n 3)
      n
      (iter 2 1 0 (- n 2)))) ;;3 ≦ nの時iter．n=1,2の時はn1,n2に渡してあるのでcountに渡す引数は(- n 2)

(define (pascals-triangle n k)
  (if (or (= k 1) (= n k))
      1
      (+ (pascals-triangle (- n 1) (- k 1))
         (pascals-triangle (- n 1) k))))

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (sine-count angle 0))

(define (sine-count angle count)
  (if (not (> (abs angle) 0.1))
      count
      (sine-count (/ angle 3) (+ count 1))))

(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt b n)
  (exp-iter b n 1))

(define (exp-iter b counter product)
  (if (= counter 0)
      product
      (exp-iter b
                (- counter 1)
                (* b product))))

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b count product)
  (cond ((= count 0) product)
        ((even? count)
         (fast-expt-iter (square b)
                        (/ count 2)
                        product))
        (else (fast-expt-iter b (- count 1) (* b product)))))

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;; 末尾再帰にしてみた
(define (* a b)
  (*iter a b 0))

(define (*iter a b sum)
  (if (= b 0)
      sum
      (*iter a (- b 1) (+ a sum))))

(define (double n)
  (* 2 n))

(define (halve n)
  (/ n 2))


(define (fast-* n m)
  (cond ((= m 0) 0)
        ((even? m) (double (fast-* n (halve m))))
        (else (+ n (fast-* n (- m 1))))))
;; fast-exptと同じように
(define (fast-* a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a b sum)
  (cond ((= b 0) sum)
        ((even? b) (fast-*-iter (double a) (halve b) sum))
        (else (fast-*-iter a (- b 1) (+ a sum)))))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (halve count)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

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

(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))


(use srfi-27)

(define (fermat-test n)
  (define a (+ 2 (random-integer (- n 2))))
  (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fermat-test n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (runtime)
    (use srfi-11)
    (let-values (((a b) (sys-gettimeofday)))
      (+ (* a 1000000) b)))

(define (search-for-primes start end)
  (cond ((> start end) (newline))
        ((even? start) (search-for-primes (+ start 1) end))
        (else (timed-prime-test start)
              (search-for-primes (+ start 2) end))))

(define (search-for-primes start end)
  (define (iter start end)
    (cond ((> start end) (newline))
          ((prime? start) (and (timed-prime-test start)
                               (iter (+ 2 start) end)))
          (else (iter (+ 2 start) end))))
  (if (odd? start)
      (iter start end)
      (iter (+ 1 start) end)))

(define (next n)
  (if (= n 2)
      3
      (+ 2 n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (carmichael-test n)
  (define (try-loop a)
    (cond ((= a n) true)
          ((= (expmod a n n) a) (try-loop (+ a 1)))
          (else false)))
  (try-loop 1))

(define (f-expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (let ((tmp (remainder (square (f-expmod b (/ e 2) m)) m)))
           (if (= 1 tmp)
               0
               tmp)))
        (else (remainder (* b (f-expmod b (- e 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let* ((tmp (expmod base (/ exp 2) m))
                  (tmp2 (remainder (square tmp) m)))
             (if (and (< 1 tmp (- n 1))
                      (= 1 tmp2))
                 0
                 tmp2)))
          (else (remainder (* base (expmod base (- exp 1) m)) m))))
  (define (try-it a)
    (= 1 (expmod a (- n 1) n)))
  (try-it (+ 1 (random-integer (- #?=n 1)))))
