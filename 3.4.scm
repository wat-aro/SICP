;; 3.4.1
(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;; 3.38
;(define balance 100)

;; Peter
;(set! balance (+ balance 10))

;; Paul
;(set! balance (- balance 20))

;; Mary
;(set! balance (- balance (/ balance 2)))

;;a ３つのプロセスがある順序で逐次的に実行された場合のbalanceの取り得る値
;;35,40,45,50

;;b プロセスが混ざり合った場合
;; 参照した後に上書きする前に他のプロセスによって値を上書きされる事がある．
;; そのために実質的に一つのプロセスしか走ってない時や二つのプロセスしか走ってない場合が起き得る
;; 新たに55,80,90,110といった値を取る事がある．
;; 90を取る例

;; 3.4.2
(use gauche.threads)

(define (delay time proc)
  (lambda ()
    (thread-sleep! time)
    (proc)))

(define (delay-print time name)
  (delay time (lambda ()
                (print name)
                name)))

(define (parallel-execute . procs)
  (let ((i 0))
    (let ((threads (map (lambda (proc)
                          (set! i (+ i 1))
                          (make-thread proc i)) procs)))
      (map thread-start! threads)
      (map thread-join! threads))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amont)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; 3.39
#|
101:P1がxに100をセットしてから，P2がxに101をセットする
121:P2がxに11をセットしてから，P1がxに121をセットする
100:P1がxから10をとってきて，そこでP2がxに11をセット，P1が続きの(* 10 10)を計算してxに100をセットする
|#

;; 3.40
#|
10^2,10^3,10^4,10^5,10^6が取り得る値となる．
直列かするとこのうち，10^6のみが残る．
|#

;; 3.41
#|
変更箇所は書き換えによる変更を行わないのでそのままでも害はないので賛成しない．
#|

;; 3.42
#|
安全な変更．並列性の間に違いはない．
|#

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

;; 3.44
(define (transfer from-account to-account amount)
  ((from-account 'withdraw) amount)
  ((to-account 'deposit) amount))

#|
交換と違い，differenceを計算する必要がないので問題はおきない
|#

;; 3.45
;; serializerが入れ子になるために無限ループする

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell #t)
             #f)))

;; 3.46
#|
cellがfalseの時に複数のプロセスが同時にcellにアクセスすればmutexは破られる
|#


;; 3.47
;; a
#|
このような形でmake-semaphoreは使われる．
|#
(define (make-serializer)
  (let ((semaphore (make-semaphore 6)))
    (lambda (p)
      (define (serialized-p . args)
        (semaphore 'acquire)
        (let ((val (apply p args)))
          (semaphore 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;;retry
            ((eq? m 'release (clear! cell)))))
    the-mutex))
#|
上を見ればわかるように評価した手続きをした後はかならずreleaseしている．
それを踏まえてmake-semaphoreを実装する．

・release
(mutex 'release)をするとcellがクリアされる．
何度clear!しても問題はないのでsemaphoreがreleaseするたびに(mutex 'release)を実行して次の処理が行えるようにする．
releaseした後にはcounterから1引いておく.

・acquire
releaseはmake-serializerのようにセマフォを使う手続きから行うのでacquire内では行わない．
counterがnと同じならば(mutex 'acquire)でロックし，カウンターを１増やす．
counterがnより大きければ(mutex 'acquire)内でretryする．
ここでカウンターを１増やさないとreleaseと数が合わなくなり，counterが負になるので1増やす．
counterがnよりも小さければcounterを１増やす．処理が終われば呼び出し元からreleaseが呼ばれる．
|#


(define (make-semaphore n)
  (let ((counter 0) (mutex (make-mutex)))
    (define (acquire)
      (cond ((<= counter n)
             (mutex 'acquire)
             (set! counter (+ counter 1)))
            ((< counter n)
             (set! counter (+ counter 1)))))
    (define (release)
      (mutex 'release)
      (set! counter (- counter 1)))
    (define (dispatch m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)
            (else
             (error "Unknown request -- MAKE-SEMAPHORE" m))))))


;; 3.48
;; make-accountの引数にnumberを追加．
;; dispatchの引数に'numberで口座番号を参照できる．
(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

;; 口座番号の小さいほうから先にserialize.
(use srfi-11)
(define (serialized-exchange account1 account2)
  (let ((id1 (account1 'id))
        (id2 (account2 'id)))
    (let-values (smaller bigger)
      (if (< id1 id2)
          (values id1 id2)
          (values id2 id1))
      (let ((serializer1 (smaller 'serializer))
            (serializer2 (bigger 'serializer)))
        ((serializer2 (serializer1 exchange))
         account1 account2)))))

;; 3.49
#|
ある口座の内容によって次にアクセスする口座の内容がかわるような状況．
具体的な状況は思い浮かばず．
|#
