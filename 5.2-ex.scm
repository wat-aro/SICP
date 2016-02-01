;; (define gcd-machine
;;   (make-machine
;;    '(a b t)
;;    (list (list 'rem remainder) (list '= =))
;;    '(test-b
;;      (test (op =) (reg b) (const 0))
;;      (branch (label gcd-done))
;;      (assign t (op rem) (reg a) (reg b))
;;      (assign a (reg b))
;;      (assign b (reg t))
;;      (goto (label test-b))
;;     gcd-done)))

;; 5.07
;; シミュレータを使い問題5.04で設計した計算機をテストせよ

;; 再帰的べき乗
;; (define factorial-recur-machine
;;   (make-machine
;;    '(b n val continue)
;;    (list (list '* *) (list '- -) (list '= =))
;;    '((assign continue (label expt-done))
;;      expt-loop
;;        (test (op =) (reg n) (const 0))
;;        (branch (label base-case))
;;        (save continue)
;;        (save n)
;;        (assign n (op -) (reg n) (const 1))
;;        (assign continue (label after-expt))
;;        (goto (label expt-loop))
;;      after-expt
;;        (restore n)
;;        (restore continue)
;;        (assign val (op *) (reg b) (reg val))
;;        (goto (reg continue))
;;      base-case
;;        (assign val (const 1))
;;        (goto (reg continue))
;;      expt-done)))

;; (set-register-contents! factorial-recur-machine 'b 2)
;; (set-register-contents! factorial-recur-machine 'n 10)
;; (start factorial-recur-machine)
;; (get-register-contents factorial-recur-machine 'val) ;=> 1024が返ってくるはず


;; ;; 反復的べき乗
;; (define factorial-iter-machine
;;   (make-machine
;;    '(b n product)
;;    (list (list '* *) (list '- -) (list '= =))
;;    '((assign product (const 1))
;;      expt-loop
;;        (test (op =) (reg n) (const 0))
;;        (branch (label fib-done))
;;        (assign n1 (op -) (reg n) (const 1))
;;        (assign p1 (op *) (reg product) (reg b))
;;        (assign n (reg n1))
;;        (assigin product (reg p1))
;;        (goto (label expt-loop))
;;      expt-done)))

;; (set-register-contents! factorial-iter-machine 'b 2)
;; (set-register-contents! factorial-iter-machine 'n 10)
;; (start factorial-iter-machine)
;; (get-register-contents factorial-iter-machine 'product)
                                        ;=>recurと同じく1024が返るはず



(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; レジスタ
(define (make-register name)
  (let ((contents '*unassaigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; スタック
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      ;; registerをregiter-tableに登録する．
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined rgister: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      ;; registerの値をregister-tableから見つける．
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      ;; pc内に保存された手続きを実行する
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)     ;the-instruction-sequenceをpcに保存してexecute
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence) ;the-instruction-sequenceにseqを登録
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations) ;新しいopをthe-opsに追加
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

;;; labelは(label-name insts)となってる．instsにはlabel-name以降の制御器の命令列がすべて入る．
;;; instsとlabelsで2つに分かれているが，(label-name insts)のinstsと引数のinstsは同じオブジェクト
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          ;; symbolであればlabel
                          (if (symbol? next-inst)
                              (if (assoc next-inst labels)
                                  (error "The same label name is used to indicate two different location " label-name)
                                  ;; (receive insts labels)なのでsymbolならlabelsにcons
                                  ;; falseならinstsにcons
                                  (receive insts
                                      (cons (make-label-entry next-inst insts)
                                            labels)))
                              ;; labelじゃない場合は(inst '())という形にconsしてからリストにしていく．
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                  labels)))))))

;;; extract-labelsでinstsとlabelsに分けたものから実行形式を作る．
;;; instsをfor-eachして実行形式を作るが，(inst . exe)という形にする．
;;; この時labelsは同じくinstsを持っているのでそれも同じ形になる．
(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

;;; update-insts!でcdrに実行形式を代入し，labelsでも同じものを指すようにするために'()とcons
(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


;; 5.08
;; start
;;  (goto (label here))
;; here
;;  (assign a (const 3))
;;  (goto (label there))
;; here
;;  (assign a (const 4))
;;  (goto (label there))
;; there

;; この時thereに達した時のaの値は何かという問題．

;; (define (extract-labels text receive)
;;   (if (null? text)
;;       (receive '() '())
;;       (extract-labels (cdr text)
;;                       (lambda (insts labels)
;;                         (let ((next-inst (car text)))
;;                           ;; symbolであればlabel
;;                           (if (symbol? next-inst)
;;                               (if (assoc next-inst labels)
;;                                   (error "The same label name is used to indicate two different location " label-name)
;;                                   ;; (receive insts labels)なのでsymbolならlabelsにcons
;;                                   ;; falseならinstsにcons
;;                                   (receive insts
;;                                       (cons (make-label-entry next-inst insts)
;;                                             labels)))
;;                               (receive (cons (make-instruction next-inst)
;;                                              insts)
;;                                   labels)))))))

;; (define (update-insts! insts labels machine)
;;   (let ((pc (get-register machine 'pc))
;;         (flag (get-register machine 'flag))
;;         (stack (machine 'stack))
;;         (ops (machine 'operations)))
;;     (for-each
;;      (lambda (inst)
;;        (set-instruction-execution-proc!
;;         inst
;;         (make-execution-procedure
;;          (instruction-text inst) labels machine
;;          pc flag stack ps)))
;;      insts)))

;; (define (make-label-entry label-name insts)
;;   (cons label-name insts))

;; (define (lookup-label labels label-name)
;;   (let ((val (assoc label-name labels)))
;;     (if val
;;         (cdr val)
;;         (error "Undefined label -- ASSEMBLE" label-name))))

;; からlabelは後に解析されたモノのほうがリストの先頭に近いところに保存されている．
;; lookup-labelではassocが使われているので先頭に近いものが先に選ばれる．
;; そのため(goto (label here))で向かうのは後のほうのhere.
;; aには4が入っている．

;; これを同じラベルを登録しようとするとエラーとなるようにする．

;; (define (make-label-entry label-name insts)
;;   (let ((val (assoc label-name labels)))
;;     (if val
;;         (error "The same label name is used to indicate two different location " label-name)
;;         (cons label-name insts))))

;; 5.2.3命令の実行手続きの生成
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

;; assign命令
(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOT instruction -- ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (tagged-list? (car e) 'label)
                    (error "Operation can be used only with registers and constants, but " e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;;5.09
;; 演算はレジスタと定数にだけ使えるという条件を強要する．
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "Operation can be used only with registers and constants -- ASSEMBLE" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;;; 5.10
(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'increment)    ;increment
         (make-increment inst machine pc))
        ((eq? (car inst) 'decrement)    ;decrement
         (make-decrement inst machine pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

;;; 選択子
(define (increment-reg-name name) (cadr name))
(define (decrement-reg-name name) (cadr name))

(define (make-increment inst machine pc)
  (let ((target (get-register machine (increment-reg-name inst))))
    (lambda ()
      (let ((value (get-contents target)))
        (cond ((number? value)
               (set-contents! target (+ value 1))
               (advance-pc pc))
              (error "INCREMENT require number, but" value))))))

(define (make-decrement inst machine pc)
  (let ((target (get-register machine (decrement-reg-name inst))))
    (lambda ()
      (let ((value (get-contents target)))
        (cond ((number? value)
               (set-contents! target (- value 1))
               (advance-pc pc))
              (error "DECREMENT require number, but" value))))))

(define add-two
  (make-machine
   '(a)
   (list )
   '(controller
     main
     (increment a)
     (increment a)
     (increment a)
     (decrement a)
     done)))

;;; 5.11-a
;;; 5.06で変更したこれを使う．
;; (controller
;;     (assign continue (label fib-done))
;;   fib-loop
;;     (test (op <) (reg n) (const 2))
;;     (branch (label immediate-answer))
;;     (save continue)
;;     (assign continue (label afterfib-n-1))
;;     (save n)
;;     (assign n (op -) (reg n) (const 1))
;;     (goto (label fib-loop))
;;   afterfib-n-1
;;     (restore n)
;;     (assign n (op -) (reg n) (const 2))
;;     (assign continue (label afterfib-n-2))
;;     (save val)
;;     (goto (label fib-loop))
;;   afterfib-n-2
;;     (assign n (reg val))                ;ここを消して
;;     (restore val)                       ;ここで(restore n)
;;     (restore continue)
;;     (assign val
;;             (op +) (reg val) (reg n))
;;     (goto (reg continue))
;;   immediate-answer
;;     (assign val (reg n))
;;     (goto (reg continue))
;;     fib-done)

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
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
     (restore n)
     (restore continue)
     (assign val
             (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

;;; 5.11-b
;;; stackに退避するときにレジスタを指定しておき，そのレジスタにresotre出来るように修正する．
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((val (pop stack)))
        ;; valのcarにregisterが入っているので呼び出し側のregと比較し#fならエラーを返す
        (cond ((eq? reg (car val))
               (set-contents! reg (cdr val))
               (advance-pc pc))
              (else
               (error "RESTORE require the same register as save, but" reg)))))))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons reg (get-contents reg))) ;regも一緒にconsする．
      (advance-pc pc))))

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
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
     (assign n (reg val))               ;ここを消して
     (restore val)                      ;ここで(restore n)
     (restore continue)
     (assign val
             (op +) (reg val) (reg n))
     (goto (reg continue))
     immediate-answer
     (assign val (reg n))
     (goto (reg continue))
     fib-done)))

;;; 5.11-c
(define (make-register name)
  (let ((contents '*unassaigned*)
        (stack (make-stack)))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'pop)
             (let ((val (stack 'pop)))
               ((dispatch 'set) val)))
            ((eq? message 'push)
             ((stack 'push) contents))
            ((eq? message 'initialize)
             (stack 'initialize))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (the-instruction-sequence '()))
    (let ((register-table
           (list (list 'pc pc) (list 'flag flag))))
      (let ((the-ops                    ;すべてのregisterに対してstackを初期化する手続きを入れる
             (list (list 'initialize-stack
                         (lambda ()
                           (for-each (lambda (stack) (stack 'initialize))
                                     register-table))))))
        ;; registerをregiter-tableに登録する．
        (define (allocate-register name)
          (if (assoc name register-table)
              (error "Multiply defined rgister: " name)
              (set! register-table
                    (cons (list name (make-register name))
                          register-table)))
          'register-allocated)
        ;; registerの値をregister-tableから見つける．
        (define (lookup-register name)
          (let ((val (assoc name register-table)))
            (if val
                (cadr val)
                (error "Unknown register: " name))))
        ;; pc内に保存された手続きを実行する
        (define (execute)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                'done
                (begin
                  ((instruction-execution-proc (car insts)))
                  (execute)))))
        (define (dispatch message)
          (cond ((eq? message 'start) ;the-instruction-sequenceをpcに保存してexecute
                 (set-contents! pc the-instruction-sequence)
                 (execute))
                ((eq? message 'install-instruction-sequence) ;the-instruction-sequenceにseqを登録
                 (lambda (seq) (set! the-instruction-sequence seq)))
                ((eq? message 'allocate-register) allocate-register)
                ((eq? message 'get-register) lookup-register)
                ((eq? message 'install-operations) ;新しいopをthe-opsに追加
                 (lambda (ops) (set! the-ops (append the-ops ops))))
                ((eq? message 'operations) the-ops)
                (else (error "Unknown request -- MACHINE" message))))
        dispatch))))

(define (make-execution-procedure inst labels machine ;引数からstackを削除
                                  pc flag ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine pc))   ;引数からstackを削除
        ((eq? (car inst) 'restore)
         (make-restore inst machine pc))  ;引数からstackを削除
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag ops)))
     insts)))

(define (make-save inst machine pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (reg 'push)
      (advance-pc pc))))

(define (make-restore inst machine pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (reg 'pop)
      (advance-pc pc))))


;;; 5.12
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels type-insts label-regs saved-regs reg-sources)
                    (update-insts! insts labels machine type-insts
                                   label-regs saved-regs reg-sources)
                    )))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '() '() '() '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels type-insts label-regs saved-regs reg-sources)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error "The same label name is used to indicate two different location " label-name)
                   (receive
                       insts
                       (cons (make-label-entry next-inst insts)
                             labels)
                     type-insts label-regs saved-regs reg-sources))
               (receive
                   (cons (make-instruction next-inst)
                         insts)
                   labels
                 (cons next-inst type-insts)
                 (add-label-reg next-inst label-regs)
                 (add-saved-reg next-inst saved-regs)
                 (add-reg-sources next-inst reg-sources))))))))

(define (add-label-reg next-inst label-regs)
  (if (and (tagged-list? next-inst 'goto)
           (tagged-list? (cadr next-inst) 'reg))
      (cons (cadadr next-inst) label-regs)
      label-regs))

(define (add-saved-reg next-inst saved-regs)
  (if (tagged-list? next-inst 'save)
      (cons (cadr next-inst) saved-regs)
      saved-regs))

(define (add-reg-sources next-inst reg-sources)
  (if (tagged-list? next-inst 'assign)
      (cons (cdr next-inst) reg-sources)
      reg-sources))

(define (tag x) (car x))

;;; 重複は既に排除されている．
(define (sort-reg reg-sources)
  (define (helper first items)
    (cond ((null? items) (list (cons (car first) (list (cdr first)))))
          ((eq? (tag first) (tag (car items)))
           (cons (cons (tag (car items)) (append (cdar items) (list (cdr first))))
                 (cdr items)))
          (else (cons (car items) (helper first (cdr items))))))
  (let recur ((lst reg-sources) (result '()))
    (cond ((null? lst) result)
          ((null? result)
           (recur (cdr lst) (list (cons (caar lst) (list (cdar lst))))))
          (else (recur (cdr lst) (helper (car lst) result))))))

(define (update-insts! insts labels machine type-insts label-regs saved-regs reg-sources)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine pc flag ops)))
     insts)
    (list insts
          (sort-reg (delete-duplicates type-insts))
          (delete-duplicates label-regs)
          (delete-duplicates  saved-regs)
          (sort-reg (delete-duplicates reg-sources)))))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    (let ((insts (assemble controller-text machine)))
      ((machine 'install-instruction-sequence) (car insts))
      ((machine 'install-instruction-types) (cadr insts))
      ((machine 'install-label-registers) (caddr insts))
      ((machine 'install-saved-registers) (cadddr insts))
      ((machine 'install-register-sources) (car (cddddr insts)))
      machine)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (the-instruction-sequence '())
        (the-instruction-types '())
        (the-label-registers '())
        (the-saved-registers '())
        (the-register-sources '()))
    (let ((register-table
           (list (list 'pc pc) (list 'flag flag))))
      (let ((the-ops
             (list (list 'initialize-stack
                         (lambda ()
                           (for-each (lambda (stack) (stack 'initialize))
                                     register-table))))))
        (define (allocate-register name)
          (if (assoc name register-table)
              (error "Multiply defined rgister: " name)
              (set! register-table
                    (cons (list name (make-register name))
                          register-table)))
          'register-allocated)
        (define (lookup-register name)
          (let ((val (assoc name register-table)))
            (if val
                (cadr val)
                (error "Unknown register: " name))))
        (define (execute)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                'done
                (begin
                  ((instruction-execution-proc (car insts)))
                  (execute)))))
        (define (dispatch message)
          (cond ((eq? message 'start)
                 (set-contents! pc the-instruction-sequence)
                 (execute))
                ((eq? message 'install-instruction-sequence)
                 (lambda (seq) (set! the-instruction-sequence seq)))
                ((eq? message 'allocate-register) allocate-register)
                ((eq? message 'get-register) lookup-register)
                ((eq? message 'install-operations)
                 (lambda (ops) (set! the-ops (append the-ops ops))))
                ((eq? message 'operations) the-ops)
                ((eq? message 'install-instruction-types)
                 (lambda (types) (set! the-instruction-types types)))
                ((eq? message 'install-label-registers)
                 (lambda (regs) (set! the-label-registers regs)))
                ((eq? message 'install-saved-registers)
                 (lambda (saved) (set! the-saved-registers saved)))
                ((eq? message 'install-register-sources)
                 (lambda (sources) (set! the-register-sources sources)))
                ((eq? message 'instruction-types) the-instruction-types)
                ((eq? message 'label-registers) the-label-registers)
                ((eq? message 'saved-registers) the-saved-registers)
                ((eq? message 'register-sources) the-register-sources)
                (else (error "Unknown request -- MACHINE" message))))
        dispatch))))

(define (get-types machine)
  (machine 'instruction-types))

(define (get-label-registers machine)
  (machine 'label-registers))

(define (get-saved-registers machine)
  (machine 'saved-registers))

(define (get-register-sources machine)
  (machine 'register-sources))

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
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
     fib-done)))

;;; 5.13
;;; register-namesを削除
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    (let ((insts (assemble controller-text machine)))
      ((machine 'install-instruction-sequence) (car insts))
      ((machine 'install-instruction-types) (cadr insts))
      ((machine 'install-label-registers) (caddr insts))
      ((machine 'install-saved-registers) (cadddr insts))
      ((machine 'install-register-sources) (car (cddddr insts)))
      machine)))

;;; lookupで見つからなければallocateで登録．
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (the-instruction-sequence '())
        (the-instruction-types '())
        (the-label-registers '())
        (the-saved-registers '())
        (the-register-sources '()))
    (let ((register-table
           (list (list 'pc pc) (list 'flag flag))))
      (let ((the-ops
             (list (list 'initialize-stack
                         (lambda ()
                           (for-each (lambda (stack) (stack 'initialize))
                                     register-table))))))
        (define (allocate-register name)
          (if (assoc name register-table)
              (error "Multiply defined rgister: " name)
              ;; 登録した後にそのレジスタを返す
              (let ((reg (make-register name)))
                   (set! register-table
                         (cons (list name reg)
                               register-table))
                   reg)))
        (define (lookup-register name)
          (let ((val (assoc name register-table)))
            (if val
                (cadr val)
                (allocate-register name)))) ;; 見つからなければ新たに登録する．
        (define (execute)
          (let ((insts (get-contents pc)))
            (if (null? insts)
                'done
                (begin
                  ((instruction-execution-proc (car insts)))
                  (execute)))))
        (define (dispatch message)
          (cond ((eq? message 'start)
                 (set-contents! pc the-instruction-sequence)
                 (execute))
                ((eq? message 'install-instruction-sequence)
                 (lambda (seq) (set! the-instruction-sequence seq)))
                ((eq? message 'allocate-register) allocate-register)
                ((eq? message 'get-register) lookup-register)
                ((eq? message 'install-operations)
                 (lambda (ops) (set! the-ops (append the-ops ops))))
                ((eq? message 'operations) the-ops)
                ((eq? message 'install-instruction-types)
                 (lambda (types) (set! the-instruction-types types)))
                ((eq? message 'install-label-registers)
                 (lambda (regs) (set! the-label-registers regs)))
                ((eq? message 'install-saved-registers)
                 (lambda (saved) (set! the-saved-registers saved)))
                ((eq? message 'install-register-sources)
                 (lambda (sources) (set! the-register-sources sources)))
                ((eq? message 'instruction-types) the-instruction-types)
                ((eq? message 'label-registers) the-label-registers)
                ((eq? message 'saved-registers) the-saved-registers)
                ((eq? message 'register-sources) the-register-sources)
                (else (error "Unknown request -- MACHINE" message))))
        dispatch))))

(define fib-machine
  (make-machine
   ;; '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(controller
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
     fib-done)))

