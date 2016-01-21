(define true #t)
(define false #f)

;; 3.5.1
(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumerate-interval a b))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-null? stream)
  (null? stream))

(define the-empty-stream '())

;; (define (delay exp)
;;   (memo-proc (lambda () exp)))
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp)))))

;; (define (cons-stream a b)
;;   (cons a
;;         (lambda () b)))
;; (define (cons-stream a b)
;;   (cons a (delay b)))
(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))


(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))


;; 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

;; 3.51
(define (show x)
  (display-line x)
  x)

;; 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ sum x))
  sum)
;; (define x (stream-map show (stream-enumerate-interval 0 10)))
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
#|
(stream-ref y 7)
(display-stream z)
メモ化しているためにseqを何回呼び出してもsumに何度も加算されることがない．
メモ化していない場合はseqを参照するたびにsumに加算されていく．
|#

;; 3.5.2 無限ストリーム
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (disible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))


(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x) (not (divisible? x (car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;; 3.53
(define s (cons-stream 1 (add-streams s s)))
;; 2^nのストリーム

;; 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials
                                               (integers-starting-from 1))))

;; 3.55
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream)
                            (partial-sums stream))))

(define sum-integers
  (partial-sums integers))

;; 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car
                                    (merge (stream-cdr s1) s2)))
                      ((> s1car s2car)
                       (cons-stream s2car
                                    (merge s1 (stream-cdr s2))))
                      (else
                       (cons-stream s1car
                                    (merge (stream-cdr s1)
                                           (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;; 3.57
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

#|
メモ化しているので(add-streams (stream-cdr fibs) fibs)の部分で加算が一回行われるだけで済んでいる．
これがメモ化していない場合はfibsの値も(stream-cdr fibs)の値も0番目と1番目の値から加算して求めなくてはならない．
|#
;; 3.58
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

#|
(/ num den)を表す少数を生成する．
(expand 1 7 10)は
1
4
2
8
5
7

(expand 3 8 10)は
3
7
5
0

つまり0.375で割り切れる．
|#

;; 3.59
;; a
;; 引数としてべき級数を表現するストリームをとり，級数の積分の定数項を除いた項の係数のストリーム
(define (integrate-series stream)
  (stream-map / stream integers))

;; b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;; 余弦の微分は正弦なので
;; cos xの微分は-sin x
(define cosine-series
  (cons-stream 0 (stream-map - (integrate-series sine-series))))

;; 正弦の微分は余弦
;; sin xの微分は cos x
(define sine-series
  (cons-stream 1 (integrate-series cosine-series)))

;; 3.60
(define (stream-head s n)
  (let iter ((s s)
             (n n))
    (if (zero? n)
        'done
        (begin
          (display (stream-car s))
          (newline)
          (iter (stream-cdr s) (- n 1))))))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define circles (add-streams (mul-series sine-series sine-series)
                             (mul-series cosine-series cosine-series)))


;; 3.61
(define (invert-unit-series stream)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s1) -1)
                           (invert-unit-series stream))))

;; 3.62
(define (div-stream s1 s2)
  (if (= s2 0)
      (error "ZERO-DIVISOR" s2)
      (mul-streams s1
                   (invert-unit-series s2))))
;; 3.5.3
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

;; 3.63
(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

;; 3.64
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (stream-limit s tolerance)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s))))
    (if (> tolerance (abs (- s1 s2)))
        s2
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;; 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

;; 3.67
;; interleave
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave (stream-map (lambda (x) (list (stream-car s) x))
                            (stream-cdr t))
                (stream-map (lambda (x) (list x (stream-car s)))
                            (stream-cdr t)))
    (pairs (stream-cdr s) (stream-cdr t)))))


;; 三つのストリームを混ぜるinterleave
(define (interleave3 s1 s2 s3)
  (if (stream-null? s1)
      (interleave s2 s3)
      (cons-stream (stream-car s1)
                   (interleave3 s2 s3 (stream-cdr s1)))))

;; interleave3を使う
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave3
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (stream-map (lambda (x) (list x (stream-car s)))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

;; 3.68
#|
(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))))
|#

;; 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (interleave (stream-map (lambda (x) (cons (stream-car s) x))
                            (pairs (stream-cdr t) (stream-cdr u)))
                (stream-map (lambda (x) (list (stream-car s) (stream-car t) x))
                            (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagoras
  (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x)))
                                (square (caddr x))))
                 (triples integers integers integers)))


;; 3.70
;; mergeを参考にして重みをつけてmerge-weightedを定義する
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car
                                    (merge (stream-cdr s1) s2)))
                      ((> s1car s2car)
                       (cons-stream s2car
                                    (merge s1 (stream-cdr s2))))
                      (else
                       (cons-stream s1car
                                    (merge (stream-cdr s1)
                                           (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (let ((w1 (weight s1car))
                      (w2 (weight s2car)))
                  (cond
                   ((< w1 w2)
                    (cons-stream s1car
                                 (merge-weighted (stream-cdr s1) s2 weight)))
                   (else
                    (cons-stream s2car
                                 (merge-weighted s1 (stream-cdr s2) weight)))))))))

;; pairsを参考にweighted-pairsを定義する
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define i+j (weighted-pairs integers integers (lambda (x) (+ (car x) (cadr x)))))

(define 2i+3j+5ij
  (weighted-pairs integers integers
                  (lambda (x) (+ (* 2 (car x))
                                 (* 3 (cadr x))
                                 (* 5 (car x) (cadr x))))))

;; 重みづけがちゃんと機能しているかを確認する
(define (stream-head-weight s n weight)
  (let iter ((s s)
             (n n))
    (if (zero? n)
        'done
        (begin
          (display (stream-car s))
          (display " : ")
          (display (weight (stream-car s)))
          (newline)
          (iter (stream-cdr s) (- n 1))))))

;; 3.71
(define (sum-cube x)
  (let ((a (car x))
        (b (cadr x)))
    (+ (* a a a) (* b b b))))

(define (ramanujan stream)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream))))
    (let ((weight1 (sum-cube s1))
          (weight2 (sum-cube s2)))
      (cond ((= weight1 weight2)
             (cons-stream weight1
                          (ramanujan (stream-cdr stream))))
            (else
             (ramanujan (stream-cdr stream)))))))

(define ramanujan-number
  (ramanujan (weighted-pairs integers integers sum-cube)))

;; 3.72
(define (sum-square x)
  (let ((a (car x))
        (b (cadr x)))
    (+ (* a a) (* b b))))

(define (triple-way-sum-square stream)
  (let ((s1 (stream-car stream))
        (s2 (stream-car (stream-cdr stream)))
        (s3 (stream-car (stream-cdr (stream-cdr stream)))))
    (let ((w1 (sum-square s1))
          (w2 (sum-square s2))
          (w3 (sum-square s3)))
      (cond ((= w1 w2 w3)
             (cons-stream w1
                          (triple-way-sum-square
                           (stream-cdr (stream-cdr stream)))))
            (else
             (triple-way-sum-square (stream-cdr stream)))))))

(define triple-way-sum-square-number
  (triple-way-sum-square (weighted-pairs integers integers sum-square)))

;; 信号としてのストリーム
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)


;; 3.73
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                           vo dt))))

;; 3.74
(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map
                          (cons proc (map stream-cdr argstreams))))))

(define zero-crossings
  (stream-map sign-change-detector sense-data
              (cons-stream 0
                           zero-crossings)))

(define (sign-change-detector scar last)
  (cond ((and (<= 0 last) (< scar 0)) -1)
        ((and (< last 0) (<= 0 scar)) 1)
        (else 0)))

;; 3.75
#|
(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (stream-cdr input-stream)
                                      avpt))))


;; s1 s2 s3という順番でストリームが流れてくる時，この手続きでは
;; s1とs2の平均a1をとり，次のs3のところでa1とs3の平均を取る．
;; s2とs3の平均をとって欲しいので引数を一つ増やす.

(define (make-zero-crossings input-stream last-value last-avpt)
  (let* ((s1 (stream-car input-stream))
         (avpt (/ (+ s1 last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      s1 avpt))))
|#
;; 3.76
(define (smooth stream)
  (cons-stream (average (car stream) 0)
               (stream-map average
                           (stream-cdr stream)
                           stream)))

(define (make-zero-crossings sense-data)
  (let ((smooth-data (smooth sense-data)))
    (stream-map sign-change-detector
                smooth-data
                (cons-stream 0 smooth-data))))

;; 3.5.4
(define int
  (cons-stream initial-value
               (add-streams (scale-stream integrand dt)
                            int)))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)


(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

;; 3.77
(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (stream-cdr integrand)
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt))))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; 3.79
(define (solve-2nd f dt h0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f y dy))
  y)

;; 3.80
(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream i (/ 1 C))
                           vo dt))))
(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(define (RLC R L C dt)
  (lambda (vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream iL (- (/ R L)))
                             (scale-stream vC (/ 1 L))))
    (stream-map (lambda (x y) (cons x y)) vC iL)))

(define RLC1 (RLC 1 1 0.2 0.1))

;; 3.5.5
(define (rand
         (let ((x random-init))
           (lambda ()
             (set! x (rand-update x))
             x))))

(use srfi-27)
(define random-init 100)
(define (rand-update x)
  (modulo (+ (* x 1103515245) 12345) 2147483647))

(define random-numbers
  (cons-stream random-init
               (stream-map rand-update random-numbers)))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

;; これを評価すると0割でエラー
;; monte-carloで一回でもtrueが出ないと分子が増えなくて0になる．
(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

;; 3.81
(use srfi-19)

(define (rand-update x)
  (modulo (+ (* x 1103515245) 12345) 2147483647))

;; 命令のストリームを引数にとる
(define (rand stream)
  (define (randoming s)
    (if (number? s)
        (random-update (time-nanosecond (current-time)))
        (random-update s)))
  (define random-stream
    (if (stream-null? stream)
        the-empty-stream
        (let ((s1 (stream-car stream)))
          (cons-stream (if (number? s1)
                           (rand-update s1)
                           (rand-update (time-nanosecond (current-time))))
                       (stream-map randoming
                                   random-stream)))))
  random-stream)

;; 3.82
(define (random-in-range x1 x2)
  (+ x1 (random-integer (- x2 x1))))


(define (estimate-integral p x1 x2 y1 y2)
  (stream-map (lambda (n) (* n (- x2 x1) (- y2 y1)))
              (monte-carlo
               (stream-map p
                           (stream-map (lambda (x) (random-in-range x1 x2)) integers)
                           (stream-map (lambda (x) (random-in-range y1 y2)) integers))
               0.0 0.0)))

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-withdraw balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw (-balance (stream-car amount-stream))
                    (stream-cdr amount-stream))))
