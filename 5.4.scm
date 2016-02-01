(load "./register-machine-simulator.scm")
(load "./eval.scm")

(eval-dispatch
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
   (test (op lambda?) (reg exp))
   (branch (label ev-lambda))
   (test (op begin?) (reg exp))
   (branch (label ev-begin))
   (test (op application?) (reg exp))
   (branch (label ev-application))
   (goto (label unknown-expression-type))

 ev-selef-eval
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
               (reg unev (reg exp) (reg env)))
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
   (restore unev)                       ;非演算子
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
   (branch (label ev-appl-last-arg))    ;最後の引数の場合はここへ飛ぶ
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
   (test (op (compound-procedure?) (reg proc)))
   (brach (label compound-apply))
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
   (brach (label ev-if-consequent))

 ev-if-alternative
   (assign exp (op if-alternative) (reg exp))
   (goto (label eval-dispatch))

 ev-if-consequent
   (assign exp (op if-consequent) (reg exp))
   (goto (label eval-dispatch))

 ev-assignment
   (assign unev (op assignment-variable) (reg exp))
   (save unev)                          ;後のために変数を退避
   (assign exp (op assignment-value) (reg exp))
   (save env)
   (save continue)
   (assign continue (label ev-assignment-1))
   (goto (label eval-dispatch))         ;代入する値を評価

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
   (save unev)                          ;後のために変数を退避
   (assign exp (op definition-value) (reg exp))
   (save env)
   (save continue)
   (assign continue (label ev-definition-1))
   (goto (label eval-dispatch))         ;定義する値を評価

 ev-definition-1
   (restore continue)
   (restore env)
   (restore unev)
   (perform
    (op define-variable!) (reg unev) (reg val) (reg env))
   (assign val (const ok))
   (goto (reg continue))
 )

;;; 5.23
(eval-dispatch
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
   (test (op cond?) (reg exp))          ;cond?を追加
   (branch (label ev-cond))
   (test (op lambda?) (reg exp))
   (branch (label ev-lambda))
   (test (op let?) (reg exp))
   (branch (label ev-let))
   (test (op begin?) (reg exp))
   (branch (label ev-begin))
   (test (op application?) (reg exp))
   (branch (label ev-application))
   (goto (label unknown-expression-type))

 ev-cond
   (assign exp (op cond->if) (reg exp))
   (goto (label eval-dispatch))

 ev-let
   (assign exp (op let->lambda) (reg exp))
   (goto (label eval-dispatch))
   )

;;; 5.24
(
 ;; unevがcondの本体を保存．expはevalされる．
 ev-cond
   (assing unev (op cond-clauses) (reg exp)) ;((p1 e1) (p2 e2) ...)の形にする．
   (save continue)                      ;cond後の継続をsave
   (save env)                           ;現在の環境をsave
   (save unev)                           ;ev-cond-loopで復元できるようにsave
   (goto (label ev-cond-test))

 ev-cond-test
   (restore exp)                        ;unevの内容がexpにコピーされる．
   (restore env)
   (restore continue)
   (test (op null?) (reg exp))
   (branch (label ev-cond-null))
   (assign exp (op car) (reg unev))      ;(p1 e1)の形に．
   (test (op cond-else-clause?))        ;(else e1)なら
   (branch (label ev-cond-else))        ;ev-cond-elseへ
   (save continue)
   (save env)
   (save unev)
   (assign continue (label ev-cond-loop)) ;eval-dispatchの後ev-cond-loopに戻れるように代入
   (assign exp (op cond-predicate) (reg exp))
   (goto (label eval-dispatch))

 ev-cond-loop
   (test (op true?) (reg val))
   (branch (label ev-cond-value))
   (restore unev)
   (restore env)                        ;環境を元に戻す
   (assign unev (op cdr) (reg unev))      ;残りのclausesへ
   (save env)
   (save unev)
   (goto (label ev-cond-test))


 ev-cond-else
   (assign exp (op cond-actions) (reg exp))
   (assign exp (op sequence->exp) (reg exp))
   (goto (label eval-dispatch))

 ev-cond-value
   ;; expはpredicateを評価した値になってる．
   (restore exp)                        ;unevが持っていたcond本体をexpがrestore
   (restore env)
   (restore continue)
   ;;((p1 e1 e1' ...) (p2 e2 e2' ...) ...)という形なのでcarを取る．
   (assign exp (op car) (reg exp))
   (assign exp (op cond-actions) (reg exp))  ;(e1 e1' ...)にする．
   (assign exp (op sequence->exp) (reg exp))
   (goto (label eval-dispatch))

 ev-cond-null
   (goto (label cond-null-error))

)
