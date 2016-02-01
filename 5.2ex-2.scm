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
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                  labels)))))))

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

;;; 5.2.4
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
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

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics) (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

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
     fib-done
     (assign n (op print-stack-statistics)))))

(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '- -) (list '* *) (list 'print print))
   '(controller
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
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
     fact-done)))

(define (fact n)
  ((fact-machine 'stack) 'initialize)
  (set-register-contents! fact-machine 'n n)
  (start fact-machine)
  (format #t "factorial: ~2d = ~10d" n (get-register-contents fact-machine 'val))
  ((fact-machine 'stack) 'print-statistics)
  (newline))

(define (display-fact n)
  (let iter ((k 1))
    (if (= k n)
        (fact k)
        (begin (fact k)
               (iter (+ k 1))))))

;;; 5.15
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-counter 0))    ;counterの追加
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
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
                (set! the-instruction-counter (+ 1 the-instruction-counter))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (initialize-counter)
        (set! instruction-counter 0))
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
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-counter) the-instruction-counter) ;counterの取得
              ((eq? message 'initilize-counter) (initilize-counter)) ;counterの初期化
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;; 5.16 instruction-tracingの実装
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-counter 0)
        (tracing-flag (lambda (inst) #f)))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
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
      (define (execute trace)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (set! the-instruction-counter (+ 1 the-instruction-counter))
                (trace (caar insts))
                ((instruction-execution-proc (car insts)))
                (execute trace)))))
      (define (trace-on)
        (set! tracing-flag (lambda (inst) (display inst) (newline)))
        'trace-on)
      (define (trace-off)
        (set! tracing-flag (lambda (inst) #f))
        'trace-off)
      (define (initialize-counter)
        (set! instruction-counter 0))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute tracing-flag))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-counter) the-instruction-counter)
              ((eq? message 'initilize-counter) (initilize-counter))
              ((eq? message 'trace-on) (trace-on))
              ((eq? message 'trace-off) (trace-off))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;; 5.17
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (if (assoc next-inst labels)
                   (error
                    "The same label name is used to indicate two different location "
                    label-name)
                   ;; ここでlabelは('label . next-inst)の形でinstsに登録
                   (let ((insts (cons (list (list 'label next-inst)) insts)))
                     (receive insts
                         (cons (make-label-entry next-inst insts)
                               labels))))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-counter 0)
        (tracing-flag (lambda (inst) #f))
        (tracing-label 'global))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
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
      (define (execute trace)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                (else
                 ((instruction-execution-proc (car insts)))
                 (cond ((label-exp? (caar insts))
                        (set! tracing-label (cdaar insts)))
                       (else (set! the-instruction-counter (+ 1 the-instruction-counter))
                             (trace (caar insts))))
                 (execute trace)))))
      (define (trace-on)
        (set! tracing-flag (lambda (inst)
                             (display tracing-label)
                             (display " : ")
                             (display inst) (newline)))
        'trace-on)
      (define (trace-off)
        (set! tracing-flag (lambda (inst) #f))
        'trace-off)
      (define (initialize-counter)
        (set! instruction-counter 0))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute tracing-flag))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-counter) the-instruction-counter)
              ((eq? message 'initilize-counter) (initilize-counter))
              ((eq? message 'trace-on) (trace-on))
              ((eq? message 'trace-off) (trace-off))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

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
        ((eq? (car inst) 'label)
         (lambda () (advance-pc pc)))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

;;; 5.18
;;; registerがtraceを持ち，trace-onがメッセージパッシングされたらトレースする．
(define (make-register name)
  (let ((contents '*unassaigned*)
        (trace (lambda (contents value) #f)))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (trace contents value)
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace (lambda (contents value)
                           (format #t "register: ~6s   oldv-value: ~s new-value: ~s\n"
                                   name contents value)))
             'trace-on)
            ((eq? message 'trace-off)
             (set! trace (lambda (contents value) #f))
             'trace-off)
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

;;; 5.19
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-instruction-counter 0)
        (tracing-flag (lambda (inst) #f))
        (tracing-label 'global)
        (breakpoint '())                ;連想リストのリスト
        (label-number 1))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
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
      (define (execute trace)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((check-breakpoint breakpoint) (format "break! ~s: ~s"
                                                       tracing-label label-number))
                (else
                 ((instruction-execution-proc (car insts)))
                 (cond ((label-exp? (caar insts))
                        (set! tracing-label (cadaar insts))
                        (set! label-number 1))
                       (else (set! the-instruction-counter (+ 1 the-instruction-counter))
                             (set! label-number (+ 1 label-number))
                             (trace (caar insts))))
                 (execute trace)))))
      ;; executeからcheck-breakpointを省いたもの．
      ;; executeから復帰するとcheck-breakpointに引っかかってまたbreakする．
      (define (proceed)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
                ((instruction-execution-proc (car insts)))
                (cond ((label-exp? (caar insts))
                       (set! tracing-label (cdaar insts))
                       (set! label-number 1))
                      (else (set! the-instruction-counter (+ 1 the-instruction-counter))
                            (set! label-number (+ 1 label-number))
                            (tracing-flag (caar insts)))))
          (execute tracing-flag)))
      (define (cancel-breakpoint label n)
        (set! breakpoint (remove (cons label n) breakpoint)))
      (define (remove x lis)
        (cond ((null? lis) (error "Cannot find in breakpoint" x))
              ((equal? x (car lis)) (cdr lis)) ;equalで連想リストと一致するか判定
              (else
               (cons (car lis) (remove x (cdr lis))))))
      (define (cancel-all-breakpoints)
        (set! breakpoint '()))
      ;; breakpointを引数に取り，再帰で一致するものがないか調べる．
      (define (check-breakpoint breakpoint)
        (cond ((null? breakpoint) #f)
              ((eq? (caar breakpoint) tracing-label)
               (cond ((eq? (cdar breakpoint) label-number) #t)
                     (else (check-breakpoint (cdr breakpoint)))))
              (else (check-breakpoint (cdr breakpoint)))))
      (define (set-breakpoint label n)
        (set! breakpoint (cons (cons label n) breakpoint)))
      (define (trace-on)
        (set! tracing-flag (lambda (inst)
                             (display tracing-label)
                             (display " : ")
                             (display inst) (newline)))
        'trace-on)
      (define (trace-off)
        (set! tracing-flag (lambda (inst) #f))
        'trace-off)
      (define (initialize-counter)
        (set! instruction-counter 0))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute tracing-flag))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-counter) the-instruction-counter)
              ((eq? message 'initilize-counter) (initilize-counter))
              ((eq? message 'trace-on) (trace-on))
              ((eq? message 'trace-off) (trace-off))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'proceed) (proceed))
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints) (cancel-all-breakpoints))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'proceed))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (trace-on machine)
  (machine 'trace-on))

(define (trace-off machine)
  (machine 'trace-off))

(define fib-machine
  (make-machine
   '(n val continue)
   (list (list '< <) (list '- -) (list '+ +))
   '(start
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
