(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;; 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))

(define y (list 'c 'd))

(define z (append x y))

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  s)

(define z (make-cycle (list 'a 'b 'c)))


;; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; mysteryはreverseと同じ結果を返し，xを先頭の要素だけを取り出したリストに置き換える．

;; 3.15
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; 3.16
;; ポインタが同じ構造を指していた場合に重複して数えてしまう．
;; さらに，循環リストの場合は結果が返ってこない．
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


;; 3.17
(define (count-pairs x)
  (define pair-list '())
  (define (recur s)
    (cond ((not (pair? x)) 0)
          ((memq s pair-list) 0)
          (else
           (set! pair-list (cons s pair-list))
           (+ (recur (car x))
              (recur (cdr x))
              1))))
  (recur s))


;; 3.18
;; 循環するリストを見つける手続き
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (cycle? x)
  (let recur ((x x)
              (record '()))
    (cond ((not (pair? x)) #f)
          ((memq x record) #t)
          (else
           (or (recur (car x) (cons x record))
               (recur (cdr x) (cons x record)))))))

;; 3.19
;; 答え見た．
;; https://github.com/nomnel/SICP/blob/master/3/19.scm
;; 一歩ずつ進むポインタと二歩ずつ進むポインタが同じになれば循環している．
;; うまいこと考えてるな
(define (look-check x)
  (define (check x0 x1)
    (cond ((eq? x0 x1) #t)
          ((null? (cdr x1)) #f)
          ((null? (cddr x1)) #f)
          (else
           (check (cdr x1) (cddr x1)))))
  (if (and (pair? x) (pair? (cdr x)))
      (check (cdr x) (cddr x))
      #f))


(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;; 3.3.2 キューの表現
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


;; 3.21
;; 項目がキューに二度挿入されているのではなく，最後に挿入した項目へ向いたポインタが二つある．
;; front-ptrの最後のポインタとrear-ptrがそう．
;; print-queue
(define (print-queue queue)
  (front-ptr queue))

;; 3.22
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  ((queue 'delete-queue!)))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-item (list item)))
        (cond ((empty-queue?)
               (set! front-ptr new-item)
               (set! rear-ptr new-item)
               front-ptr)
              (else
               (set-cdr! rear-ptr new-item)
               (set! rear-ptr new-item)
               front-ptr))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE called with an empty queue" front-ptr))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!)
             insert-queue!)
            ((eq? m 'delete-queue!)
             delete-queue!)
            (else
             (error "Undefined operation -- MAKE-QUEUE" m))))
    dispatch))

;; 3.23
;; dequeの実装
(define (value-ptr ptr) (caar ptr))
(define (prev-ptr ptr) (cdar ptr))
(define (next-ptr ptr) (cdr ptr))

;; ((value))というリストを作る
(define (make-ptr value) (list (list value)))

(define (make-queue)
  (cons '() '()))

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (empty-queue? queue)
  (null? (front-queue queue)))

(define (make-empty-queue queue)
  (set-front-ptr! queue '())
  (set-rear-ptr! queue '())
  queue)

(define (printing queue)
  (let recur ((deque (front-ptr queue)))
    (cond ((null? deque) '())
          (else
           (cons (value-ptr deque)
                 (recur (next-ptr deque)))))))

(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (set-prev-ptr! ptr item) (set-cdr! (car ptr) item))
(define (set-next-ptr! ptr item) (set-cdr! ptr item))

(define (front-insert-queue! queue item)
  (let ((new-item (make-ptr item)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           'ok)
          (else
           (set-prev-ptr! (front-queue queue)
                          new-item)
           (set-next-ptr! new-item
                          (front-queue queue))
           (set-front-ptr! queue new-item)
           'ok))))

(define (rear-insert-queue! queue item)
  (let ((new-item (make-ptr item)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-item)
           (set-rear-ptr! queue new-item)
           'ok)
          (else
           (set-next-ptr! (rear-queue queue)
                          new-item)
           (set-prev-ptr! new-item
                          (rear-queue queue))
           (set-rear-ptr! queue new-item)
           'ok))))

(define (front-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "FRONT-DELETE! called with an empty queue" queue))
        (else
         (let* ((old-front-ptr (front-ptr queue))
                (new-front-ptr (next-ptr old-front-ptr)))
           (cond ((null? new-front-ptr)
                  (make-empty-queue queue)
                  (value-ptr old-front-ptr))
                 (else
                  (set-next-ptr! old-front-ptr
                                 '())
                  (set-prev-ptr! new-front-ptr
                                 '())
                  (set-front-ptr! queue new-front-ptr)
                  (value-ptr old-front-ptr)))))))

(define (rear-delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "REAR-DELETE! called with an empty queue" queue))
        (else
         (let ((new-rear-ptr (prev-ptr (rear-ptr queue)))
               (old-rear-ptr (rear-ptr queue)))
           (cond ((null? new-rear-ptr)
                  (make-empty-queue queue)
                  (value-ptr old-rear))
                 (else
                  (set-prev-ptr! old-rear-ptr
                                 '())
                  (set-next-ptr! new-rear-ptr
                                 '())
                  (set-rear-ptr! queue new-rear-ptr)
                  (value-ptr old-rear-ptr)))))))
;; 3.3.3
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key value records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;; 二次元の表
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr subtable
                       (cons (cons key-2 value)
                             (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; 3.24
(define (make-table same-key?)
  (define (assoc key value records)
    (cond ((null? records) #f)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; 3.25
;; keyではなくkey-listを'(x y z)という形で渡す
(define (make-table)
  (let ((local-table (list '*local-table*)))
    (define (lookup key-list)
      (let loop ((key-list key-list)
                 (table local-table))
        (if (null? key-list)
            false
            (let ((record (assoc (car key-list) (cdr table))))
              (and record
                   (if (null? (cdr key-list))
                       record
                       (loop (cdr key-list) recordf)))))))
    (define (insert! key-list value)
      (let loop ((key-list key-list)
                 (table local-table))
        (if (null? key-list)
            false
            (let ((record (assoc (car key-list) (cdr table))))
              (if record
                  (if (null? (cdr key-list))
                      (set-cdr! record value)
                      (loop (cdr key-list)
                            record))
                  (set-cdr! table
                            (cons (insert-iter! key-list value)
                                  (cdr table)))))))
      'ok)
    (define (isnert-iter! key-list value)
      (if (null? (cdr key-list))
          (cons (car key-list) value)
          (list (car key-list) (insert-iter! (cdr key-list value)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m)))))
  dispatch)

;; 3.26
(define (make-table)
  ;; tree
  (define (make-tree key value left-branch right-branch)
    (list key value left-branch right-branch))
  ;; 選択子
  (define (key-tree tree) (car tree))
  (define (value-tree tree) (cadr tree))
  (define (left-branch tree) (caddr tree))
  (define (right-branch tree) (cadddr tree))
  ;; set
  (define (set-value! value tree)
    (set-car! (cdr tree) value))
  (define (set-left-branch! left tree)
    (set-car! (cddr tree) left))
  (define (set-right-branch! right tree)
    (set-car! (cdddr tree) right))

  (let ((local-table (make-tree '*table* '() '() '())))
    (define (lookup key-list)
      (let iter ((key-list key-list)
                 (table local-table))
        (cond ((null? table) false)
              ((= (car key-list) (key-tree table))
               (if (null? (cdr key-list))
                   table
                   (iter (cdr key-list) (value-tree table))))
              ((< (car key-list) (key-tree table))
               (iter key-list (left-branch table)))
              ((> (car key-list) (key-tree table))
               (iter key-list (right-branch table))))))

    (define (insert! key-list value)
      (let iter ((key-list key-list)
                 (table local-table))
        (cond ((eq? (key-tree local-table) '*table*)
               (set! local-table (insert-iter! key-list value)))
              ((= (car key-list) (key-tree table))
               (set-value! (insert-iter! key-list value) table))
              ((< (car key-list) (key-tree table))
               (if (null? (left-branch table))
                   (set-left-branch! (insert-iter! key-list value) table)
                   (iter key-list (left-branch table))))
              ((> (car key-list) (key-tree table))
               (if (null? (right-branch table))
                   (set-right-branch! (insert-iter! key-list value) table)
                   (iter key-list (right-branch table))))))
      'done)

    (define (insert-iter! key-list value)
      (if (null? (cdr key-list))
          (make-tree (car key-list) value '() '())
          (make-tree (car key-list)
                     (insert-iter! (cdr key-list) value) '() '())))

    (define (printing)
      (display local-table)
      (newline))

    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'printing) (printing))
            (else ((error "Unknown operation --TABLE" m)))))
  dispatch))

(define (lookup key-list table)
  ((table 'lookup) key-list))

(define (insert! key-list value table)
  ((table 'insert!) key-list value))

(define (printing table)
  (table 'printing))

;; 3.27
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key record)
  (cond ((null? record) false)
        ((equal? key (caar record)) (car record))
        (else (assoc key (cdr record)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))

(define (make-table)
  (list '*table*))



(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              (display "x : ")
              (display x)
              (newline)
              (display "table : ")
              (display table)
              (newline)
              result))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))


(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))


(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; 3.29
(define (or-gate a1 a2 output)
  (let ((b1 (make-wire))
        (b2 (make-wire))
        (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output)))
;; 遅延時間は(+ and-gate-delay (* 2 inverter-delay))

;; 3.30
;; 最後のfull-adderのc-inは0．
;; (make-wire)の初期値は0と仮定してます．
(define (ripple-carry-adder Ak Bk Sk C)
  (let ((c-in (make-wire)))
    (cond ((null? (cdr Ak))
           (full-adder (car Ak) (car Bk) c-in (car Sk) C)
           'ok)
          (else
           (full-adder (car Ak) (car Bk) c-in (car Sk) C)
           (ripple-carry-adder (cdr Ak) (cdr Bk) (make-wire) (cdr Sk) c-in)))))

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedure)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agneda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedure)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)

;; 3.31
;; accept-action-procedure!でprocを実行して初期化している部分で初期化しないとどうなるか．
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc)) ;;この(proc)がないとどうなるか

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

;; and-gate手続きをつかって述べる．
;; ここでprocがないとここの内部定義and-action-procedureを登録しているだけで実行しない．
;; つまりafter-delayも実行しない．
(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; after-delay手続きでthe-agendaに登録している．
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;; propagateはthe-agendaに登録されたactionを一つずつ実行する．
;; after-delayが呼ばれていないので何も登録されていないため何も実行できない．
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;; agendaの実装
(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segments time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; 3.32
;; A->B->Cという順に並んだ回線があったとする．
;; FIFOの場合Aが変化するとそれがBに伝わり，次のactionが実行されCに伝わる．
;; FILOの場合Aが変化してもまずB-C間のactionが実行されCは変化しない．
;; そのあとA-B間のactionが実行されるAの変化がBに伝わる．
;; FILOの場合は最後まで変化が伝わらないためFIFOが使われている．

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-vlaue))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

;; 3.33
(define (averager a b c)
  (let ((x (make-connector))
        (p (make-connector)))
    (adder a b p)
    (multiplier c x p)
    (constant 2 x)
    'ok))

;; 3.34
(define (squarer a b)
  (multiplier a a b))

;; bを求める場合はうまく動く．aが定まれば(* a a)の値がbに伝わる．
;; aを求める時にはこれは動かない．
;; multiplierは第一引数を求める時に(/ 第三引数 第二引数)を計算するが，
;; 今の場合は第二引数がわからない．
;; 同様に第二引数も求められない．

;; 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-balue b))
            (set-value! a (sqrt b) me))
        (set-value! b (square a) me)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  me)

;; 3.36
(define a (make-connector))
(define b (make-connector))
(set-value! a 10 'user)

;; 3.37
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder x z y)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier x z y)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))
