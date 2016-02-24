;;; 5.2 で実装したシミュレータ．トレース機能追加済み．
;;; 初めてassignするレジスタを登録していくタイプ
(load "./register-machine-simulator.scm")
;;; 4.1で実装した評価器
(load "./eval.scm")

;;; 5.4.1-5.4.4までの注釈でかかれていた手続き
(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)
(define (no-more-exps? seq) (null? seq))
(define (empty-arglist) '())
(define (adjoin-arg arg arg-list)
  (append arg-list (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
        (else (display object))))

(define eceval-procedure
  (list (list 'self-evaluating? self-evaluating?)
         (list 'variable? variable?)
         (list 'quoted? quoted?)
         (list 'text-of-quotation text-of-quotation)
         (list 'assignment? assignment?)
         (list 'definition? definition?)
         (list 'if? if?)
         (list 'true? true?)
         (list 'cond? cond?)
         (list 'lambda? lambda?)
         (list 'begin? begin?)
         (list 'application? application?)
         (list 'lookup-variable-value lookup-variable-value)
         (list 'lambda-parameters lambda-parameters)
         (list 'lambda-body lambda-body)
         (list 'make-procedure make-procedure)
         (list 'operands operands)
         (list 'operator operator)
         (list 'empty-arglist empty-arglist)
         (list 'no-operands? no-operands?)
         (list 'first-operand first-operand)
         (list 'last-operand? last-operand?)
         (list 'adjoin-arg adjoin-arg)
         (list 'rest-operands rest-operands)
         (list 'primitive-procedure? primitive-procedure?)
         (list 'compound-procedure? compound-procedure?)
         (list 'apply-primitive-procedure apply-primitive-procedure)
         (list 'procedure-parameters procedure-parameters)
         (list 'procedure-environment procedure-environment)
         (list 'extend-environment extend-environment)
         (list 'procedure-body procedure-body)
         (list 'begin-actions begin-actions)
         (list 'first-exp first-exp)
         (list 'last-exp? last-exp?)
         (list 'rest-exps rest-exps)
         (list 'if-predicate if-predicate)
         (list 'if-alternative if-alternative)
         (list 'if-consequent if-consequent)
         (list 'cond->if cond->if)
         (list 'and? and?)
         (list 'and->if and->if)
         (list 'or? or?)
         (list 'or->if or->if)
         (list 'let? let?)
         (list 'let->combination let->combination)
         (list 'let*? let*?)
         (list 'let*->nested-lets let*->nested-lets)
         (list 'letrec? letrec?)
         (list 'letrec->let letrec->let)
         (list 'assignment-variable assignment-variable)
         (list 'assignment-value assignment-value)
         (list 'set-variable-value! set-variable-value!)
         (list 'definition-variable definition-variable)
         (list 'definition-value definition-value)
         (list 'define-variable! define-variable!)
         (list 'prompt-for-input prompt-for-input)
         (list 'read read)
         (list 'get-global-environment get-global-environment)
         (list 'announce-output announce-output)
         (list 'user-print user-print)
         (list 'compiled-procedure? compiled-procedure?)
         (list 'compiled-procedure-entry compiled-procedure-entry)
         (list 'make-compiled-procedure make-compiled-procedure)
         (list 'compiled-procedure-env compiled-procedure-env)
         (list 'lexical-address-lookup lexical-address-lookup)
         (list 'lexical-address-set! lexical-address-set!)
         (list '= =)
         (list '+ +)
         (list '- -)
         (list '* *)
         (list '/ /)
         (list 'false? false?)
         (list 'list list)
         (list 'cons cons)))

(define eceval
  (make-machine
   eceval-procedure
   '(start
     (assign compapp (label compound-apply)) ;追加
     (branch (label external-entry))
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform
      (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val))

     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op cond?) (reg exp))        ;cond?を追加
     (branch (label ev-cond))
     (test (op and?) (reg exp))
     (branch (label ev-and))
     (test (op or?) (reg exp))
     (branch (label ev-or))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op let*?) (reg exp))
     (branch (label ev-let*))
     (test (op letrec?) (reg exp))
     (branch (label ev-letrec))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))

     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))

     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))

     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))

     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev)                     ;非演算子
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))  ;最後の引数の場合はここへ飛ぶ
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg)) ;evalの後にここにいって引数を回復する．
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))

     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc))
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))

     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))

     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))

     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))

     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))

     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))

     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))

     ev-cond
     (assign exp (op cond->if) (reg exp))
     (goto (label eval-dispatch))

     ev-and
     (assign exp (op and->if) (reg exp))
     (goto (label eval-dispatch))

     ev-or
     (assign exp (op or->if) (reg exp))
     (goto (label eval-dispatch))

     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))

     ev-let*
     (assign exp (op let*->nested-lets) (reg exp))
     (goto (label eval-dispatch))

     ev-letrec
     (assign exp (op letrec->let) (reg exp))
     (goto (label eval-dispatch))

     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)                        ;後のために変数を退避
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))       ;代入する値を評価

     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)                        ;後のために変数を退避
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))       ;定義する値を評価

     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))

     print-result
     (perform
      (op print-stack-statistics))
     (perform
      (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue)                 ;スタックを掃除する
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     )))
