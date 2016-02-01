(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)

;; 5.02
(controller
 factorial
   (assign n (op read))
   (assign product (const 1))
   (assign counter (const 1))
 test-b
   (test (op >) (reg counter) (reg n))
   (branch (label factorial-done))
   (assign t (op mul) (reg product) (reg counter))
   (assign u (op sum) (reg counter) (const 1))
   (assign product (reg t))
   (assign counter (reg u))
   (goto (label test))
 factorial-done)

;; 入力を読み込み結果を印字するGCD計算機
(condtroller
 gcd-loop
   (assign a (op read))
   (assign b (op read))
 test-b
   (test (op =) (reg b) (const 0))
   (branch (lambel gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-mode
   (perform (op pront) (reg a))
   (goto (label gcd-loop)))

(controller
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (reg a))
  rem-loop
    (test (op <) (reg t) (reg b))
    (branch (label rem-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
  rem-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
gcd-done)


(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

;; 5.03
(define (sqrt n)
  (sqrt-iter 1.0 x))

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


;; good-enough?, improveを使った場合
(controller
 sqrt
   (assign x (op read))
   (assign guess (const 1.0))
 test-b
   (test (op good-enough?) (reg guess) (reg x))
   (branch (label sqrt-done))
   (assign t (op improve) (reg guess) (reg x))
   (assign guess (reg t))
   (goto (label test-b))
 sqrt-done
   (perform (op peinr) (reg guess)))

;; good-enough?, improveを使わずに
(controller
  sqrt
    (assign x (op read))
    (assign guess (const 1.0))
  good-enough?
    (assign p (op square) (reg guess))
    (assign diff (op -) (reg p) (reg x))
    (assign g (op abs) (reg diff))
    (test (op <) (reg g) (const 0.001))
    (branch (label sqrt-done))
  improve
    (assign d (op /) (reg x) (reg guess))
    (assign t (op average) (reg d) (reg guess))
    (assign guess (reg t))
  sqrt-done
  (perform (op print) (reg guess)))

;; 5.1.4
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))


;; 再帰的階乗計算機
(controller
    (assign continue (label fact-done))
  fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; nと continue を退避し再帰呼び出しを設定する．
    ;; 再帰呼び出しから戻るとき after-fact から
    ;; 計算が続行するように continue を設定
    (save continu)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (lavel fact-loop))
  after-fact
    (restore n)
    (restore continu)
    (assign val (op *) (reg n) (reg val))
    (goto (reg coninue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  fact-done)


;; fib
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; Fibonacci 数を計算する計算機の制御器
(controller
    (assign continue (label fib-done))
  fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;; Fib(n-1)を計算するよう設定
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)                            ;n の昔の値を退避
    (assign n (op -) (reg n) (const 1)) ;n を n-1 に変える
    (goto (label fib-loop))             ;再帰呼び出しを実行
  afterfib-n-1                          ;戻った時 Fib(n-1) は val にある
    (restore n)
    (restore continue)
    ;; Fib(n-2)を計算するよう設定
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)                          ;Fib(n-1) を退避
    (goto (label fib-loop))
  afterfib-n-2                          ;戻った時 Fib(n-2) の値は val にある
    (assign n (reg val))                ;n には Fib(n-2) がある
    (restore val)                       ;val には Fib(n-1) がある
    (restore continue)
    (assign val                         ;Fib(n-1) + Fib(n-2)
            (op +) (reg val) (reg n))
    (goto (reg continue))                 ;呼び出し側に戻る．答えは val にある
  immediate-answer
    (assign val (reg n))                ;基底の場合: Fib(n)=n
    (goto (reg continue))
  fib-done)


;; 5.04
;; a 再帰的べき乗
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(controller
   (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
base-case
   (assign val (const 1))
   (goto (reg continue))
expt-done)

;; b 反復的べき乗
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
        product
        (expt-iter (- counter 1) (* product))))
  (expt-iter n 1))

(contoroller
   (assign product (const 1))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label fib-done))
   (assign n1 (op -) (reg n) (const 1))
   (assign p1 (op *) (reg product) (reg b))
   (assign n (reg n1))
   (assigin product (reg p1))
   (goto (label expt-loop))
 expt-done)


;; 5.05
;; 再帰的な階乗計算を机上シミュレートする．
(controller
    (assign continue (label fact-done))
  fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; nと continue を退避し再帰呼び出しを設定する．
    ;; 再帰呼び出しから戻るとき after-fact から
    ;; 計算が続行するように continue を設定
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
  after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  fact-done)

;; (assign (reg n) (const 3))を既に実行済みであると仮定する．

(assign continue (label fact-done))     ;continue <= fact-done

(test (op =) (reg n) (const 1))         ;(= 3 1) => #f

(save continue)                         ;fact-done => stack => fact-done

(save n)                                ;3 => stack => 3, fact-done

(assign n (op -) (reg n) (const 1))     ;n <= 2

(assign continue (label after-fact))    ;continue <= after-fact

(goto (label fact-loop))

(test (op =) (reg n) (const 1))         ;(= 2 1) => #f

(save continue)                         ;after-fact => stack => after-fact, 3, fact-done

(save n)                                ;2 => stack => 2, after-fact, 3, fact-done

(assign n (op -) (reg n) (const 1))     ;n <= 1

(assign continue (label after-fact))    ;continue <= after-fact

(goto (label fact-loop))

(test (op =) (reg n) (const 1))         ;(= 1 1) => #t

(branch (label base-case))

(assign val (const 1))                  ;val <= 1

(goto (reg continue))                   ;goto after-fact

(restore n)                             ;n <= 2 | stack => after-fact, 3, fact-done

(restore continue)                      ;continue <= after-fact | stack => 3, fact-done

(assign val (op *) (reg n) (reg val))   ;val <= 2 <= (* 2 1)

(goto (reg continue))                   ;goto after-fact

(restore n)                             ;n <= 3 | stack fact-done

(restore continue)                      ;continue <= fact-done | stack => null

(assign val (op *) (reg n) (reg val))   ;n <= 6 <= (* 2 3)

(goto (reg continue))                   ;goto fact-done

fact-done

;; 次はfibonacci計算を机上シミュレートする．
;; Fibonacci 数を計算する計算機の制御器
(controller
    (assign continue (label fib-done))
  fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    ;; Fib(n-1)を計算するよう設定
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)                            ;n の昔の値を退避
    (assign n (op -) (reg n) (const 1)) ;n を n-1 に変える
    (goto (label fib-loop))             ;再帰呼び出しを実行
  afterfib-n-1                          ;戻った時 Fib(n-1) は val にある
    (restore n)
    (restore continue)
    ;; Fib(n-2)を計算するよう設定
    (assign n (op -) (reg n) (const 2))
    (save continue)
    (assign continue (label afterfib-n-2))
    (save val)                          ;Fib(n-1) を退避
    (goto (label fib-loop))
  afterfib-n-2                          ;戻った時 Fib(n-2) の値は val にある
    (assign n (reg val))                ;n には Fib(n-2) がある
    (restore val)                       ;val には Fib(n-1) がある
    (restore continue)
    (assign val                         ;Fib(n-1) + Fib(n-2)
            (op +) (reg val) (reg n))
    (goto (reg continue))                 ;呼び出し側に戻る．答えは val にある
  immediate-answer
    (assign val (reg n))                ;基底の場合: Fib(n)=n
    (goto (reg continue))
    fib-done)

;; 階乗と同じく(assign n (const 3))を実行済みと仮定する
(assign continue (label fib-done))      ;continue <= fib-done

(test (op <) (reg n) (const 2))         ;(< 3 2) => #f

(save continue)                         ;fib-done => stack => fib-done

(assign continue (label afterfib-n-1))  ;continue <= afterfib-n-1

(save n)                                ;3 => stack => 3, fib-done

(assign n (op -) (reg n) (const 1))     ;n <= 2

(goto (label fib-loop))

(test (op <) (reg n) (const 2))         ;(< 2 2) => #f

(save continue)                         ;afterfib-n-1 => stack => afterfib-n-1, 3, fib-done

(assign continue (label afterfib-n-1))  ;continue <= afterfib-n-1

(save n)                                ;2 => stack => 2, afterfib-n-1, 3, fib-done

(assign n (op -) (reg n) (const 1))     ;n <= 1

(goto (label fib-loop))

(test (op <) (reg n) (const 2))         ;(< 1 2) => #t

(branch (label immediate-answer))

(assign val (reg n))                    ;val <= 1

(goto (reg continue))                   ;(goto afterfib-n-1)

(restore n)                             ;n <= 2 | stack => afterfib-n-1, 3, fib-done

(restore continue)                      ;continue <= afterfib-n-1 | stack => 3, fib-done

(assign n (op -) (reg n) (const 2))     ;n <= 0

(save continue)                         ;afterfib-n-1 => stack =>afterfib-n-1, 3, fib-done

(assign continue (label afterfib-n-2))  ;continue <= afterfib-n-2

(save val)                              ;1 => stack => 1, afterfib-n-1, 3, fib-done

(goto (label fib-loop))

(test (op <) (reg n) (const 2))         ;(< 0 2) => #t

(branch (label immediate-answer))

(assign val (reg n))                    ;val <= 0

(goto (reg continue))                   ;(goto afterfib-n-2)

(assign n (reg val))                    ;n <= 0

(restore val)                           ;val <= 1 | stack => afterfib-n-1, 3, fib-done

(restore continue)                      ;continue <= afterfib-n-1 | stack => 3, fib-done

(assign val (op +) (reg val) (reg n))   ;val <= 1 <= (+ 1 0)

(goto (reg continue))                   ;(goto afterfib-n-1)

(restore n)                             ;n <= 3 | stack => fib-done

(restore continue)                      ;continue <= fib-done | stack => null

(assign n (op -) (reg n) (const 2))     ;n <= 1 <= (- 3 2)

(save continue)                         ;fib-done => stack => fib-done

(assign continue (label afterfib-n-2))  ;continue <= afterfib-n-2

(save val)                              ;1 => stack => 1, fib-done

(goto (label fib-loop))

(test (op <) (reg n) (const 2))         ;(< 1 2) => #t

(brach (label immediate-answer))

(assign val (reg n))                    ;val <= 1

(goto (reg continue))                   ;(goto afterfib-n-2)

(assign n (reg val))                    ;n <= 1

(restore val)                           ;val <= 1 | stack => fib-done

(restore continue)                      ;continue <= fib-done

(assign val (op +) (reg val) (reg n))   ;val <= 2 <= (+ 1 1)

(goto (reg continue))                   ;(goto fib-done)

fib-done

;; 5.06
;; Fibonacci計算機の余分なsaveとrestoreを取り除く

  afterfib-n-1
    (restore n)
    (restore continue)                  ;ここでcontinueをrestoreしているのに
    (assign n (op -) (reg n) (const 2))
    (save continue)                     ;ここでそのままcontinueをsaveして
    (assign continue (label afterfib-n-2)) ;ここでcontinueを上書きしている．
    (save val)
    (goto (label fib-loop))

整理すると
  afterfib-n-1
    (restore n)
    (assign n (op -) (reg n) (const 2))
    (assign continue (label afterfib-n-2))
    (save val)
    (goto (label fib-loop))
これでよい．
全文は以下の通りになる．

(controller
    (assign continue (label fib-done))
  fib-loop
    (test (op <) (reg n) (const 2))
    (branch (label immediate-answer))
    (save continue)
    (assign continue (label afterfib-n-1))
    (save n)
    (assign n (op -) (reg n) (const 1))
    (goto (label fib-loop))
  afterfib-n-1
    (restore n)
    (assign n (op -) (reg n) (const 2))
    (assign continue (label afterfib-n-2))
    (save val)
    (goto (label fib-loop))
  afterfib-n-2
    (assign n (reg val))
    (restore val)
    (restore continue)
    (assign val
            (op +) (reg val) (reg n))
    (goto (reg continue))
  immediate-answer
    (assign val (reg n))
    (goto (reg continue))
    fib-done)
