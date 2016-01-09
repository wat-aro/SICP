(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amont))
             balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

;; 3.1
(define (make-accumulator n)
  (let ((sum n))
    (lambda (num)
      (set! sum (+ sum num))
      sum)))

;; 3.2
(define (make-monitored f)
  (let ((mf 0))
    (lambda (in)
      (cond ((eq? in 'how-many-calls?) mf)
            ((eq? in 'reset-count)
             (set! mf 0))
            (else (set! mf (+ 1 mf))
                  (f in))))))

;; 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (login-error amount) "Incorrect password")
  (define (dispatch pass m)
    (if (eq? pass password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        login-error))
  dispatch)

;; 3.4
(define (make-account balance password)
  (let ((counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (login-error amount)
      (set! counter (+ 1 counter))
      (if (>= counter 7)
          (coll-the-cop)
          "Incorrect password"))
    (define (call-the-cops)
      "110")
    (define (dispatch pass m)
      (if (eq? pass password)
          (cond ((eq? m 'withdraw)
                 (set! counter 0)
                 withdraw)
                ((eq? m 'deposit)
                 (set! counter 0)
                 deposit)
                (else (error "Unknown request: MAKE-ACCOUNT"
                             m)))
          login-error))
    dispatch))

;; 3.1.2
(use srfi-27)

(define random-init 100)

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))


;; 3.5
(use srfi-27)

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random-real) range))))
;; 問題分には(+ low (random range))となっている．

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((x-length (- x2 x1))
        (y-length (- y2 y1)))
    (let ((square-area (* x-length y-length)))
      (* square-area (monte-carlo trials
                                  (lambda () (P (random-in-range x1 x2)
                                                (random-in-range y1 y2))))))))

(estimate-integral (lambda (x y)
                           (<= (+ (square x) (square y)) 1))
                   -1.0 1.0 -1.0 1.0 10000)

;; 3.6

;; オリジナルのrand
(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

;; 'generateで乱数生成，'resetで引数の数字で初期化するrand
(define rand
  (let ((x random-init))
    (define (reset new-rand)
      (set! x new-rand)
      x)
    (define (generate)
      (set! x (rand-update x)))
    (define (dispatch m)
      (cond ((eq? m 'reset)
             reset)
            ((eq? m 'generate)
             (generate))
            (else
             (error "Unknown argument -- RAND" (list m)))))
    dispatch))

;; 3.1.3
(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;; (define peter-acc (make-account 100))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))

(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (befin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))

;; 3.7
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (login-error amount) "Incorrect password")
  (define (dispatch pass m)
    (if (eq? pass password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request: MAKE-ACCOUNT"
                           m)))
        login-error))
  dispatch)

(define (make-joint account password new-account-password)
  (define (dispatch entered-pass m)
    (if (eq? entered-pass new-account-password)
        (account password m)
        "Incorrect password"))
  dispatch)

(define peter-acc
  (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

;; 3.08
(define f
  (let ((a 1))
    (lambda (x)
      (set! a (* a x))
      a)))

(define f
  ((lambda (a)
    (lambda (x)
      (set! a (* a x))
      a)) 1))

;; 3.09
;; 再帰の場合
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))


(define factorial
  (lambda (n)
    (fact-iter 1 1 n)))

(define fact-iter
  (lambda (product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count))))

;; 3.10
(define w1 (make-withdraw 100))
;; E2(balance:100)->E1(initial-amount:100)->global

(w1 50)
;; E3->(amount:50)->E2(balance:50)->E1(initial-amount:100)->global

(define (w2 (make-withdraw 100)))
;; E5(balance:100)->E4(initial-amount:100)->global
