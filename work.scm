((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry232) (reg env))
  (goto (label after-lambda233))
  entry232
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op =) (reg arg1) (reg arg2))
  (test (op false?) (reg val))
  (branch (label false-branch235))
  true-branch234
  (assign val (const 1))
  (goto (reg continue))
  false-branch235
  (save continue)                       ;replに戻るcontinue
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const factorial) (reg proc))
  (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign arg2 (const 1))
  (assign val (op -) (reg arg1) (reg arg2))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch237))
  compiled-branch238
  (assign continue (label proc-return240)) ;continue: proc-return240
  (assign val (op compiled-procedure-entry) (reg proc)) ;factorialのentry
  (goto (reg val))
  proc-return240
  (assign arg1 (reg val))
  (goto (label after-call239))
  primitive-branch237
  (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call239
  (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign val (op *) (reg arg1) (reg arg2))
  (restore continue)
  (goto (reg continue))
  after-if236
  after-lambda233
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))))

read-eval-print-loop : (perform (op prompt-for-input) (const))
;;; EC-Eval input:))
(factorial 3)
read-eval-print-loop : (assign exp (op read))
read-eval-print-loop : (assign env (op get-global-environment))
read-eval-print-loop : (assign continue (label print-result)) ;continue: print-result
read-eval-print-loop : (goto (label eval-dispatch))
eval-dispatch : (test (op self-evaluating?) (reg exp))
eval-dispatch : (branch (label ev-self-eval))
eval-dispatch : (test (op variable?) (reg exp))
eval-dispatch : (branch (label ev-variable))
eval-dispatch : (test (op quoted?) (reg exp))
eval-dispatch : (branch (label ev-quoted))
eval-dispatch : (test (op assignment?) (reg exp))
eval-dispatch : (branch (label ev-assignment))
eval-dispatch : (test (op definition?) (reg exp))
eval-dispatch : (branch (label ev-definition))
eval-dispatch : (test (op if?) (reg exp))
eval-dispatch : (branch (label ev-if))
eval-dispatch : (test (op cond?) (reg exp))
eval-dispatch : (branch (label ev-cond))
eval-dispatch : (test (op and?) (reg exp))
eval-dispatch : (branch (label ev-and))
eval-dispatch : (test (op or?) (reg exp))
eval-dispatch : (branch (label ev-or))
eval-dispatch : (test (op lambda?) (reg exp))
eval-dispatch : (branch (label ev-lambda))
eval-dispatch : (test (op let?) (reg exp))
eval-dispatch : (branch (label ev-let))
eval-dispatch : (test (op let*?) (reg exp))
eval-dispatch : (branch (label ev-let*))
eval-dispatch : (test (op letrec?) (reg exp))
eval-dispatch : (branch (label ev-letrec))
eval-dispatch : (test (op begin?) (reg exp))
eval-dispatch : (branch (label ev-begin))
eval-dispatch : (test (op application?) (reg exp))
eval-dispatch : (branch (label ev-application))
ev-application : (save continue)        ;stack: print-result
ev-application : (save env)             ;stack: glotal print-result
ev-application : (assign unev (op operands) (reg exp))
ev-application : (save unev)            ;stack: operands global print-result
ev-application : (assign exp (op operator) (reg exp)) ;exp op
ev-application : (assign continue (label ev-appl-did-operator)) ;continue: ev-appl-did-operator
ev-application : (goto (label eval-dispatch))
eval-dispatch : (test (op self-evaluating?) (reg exp))
eval-dispatch : (branch (label ev-self-eval))
eval-dispatch : (test (op variable?) (reg exp))
eval-dispatch : (branch (label ev-variable))
ev-variable : (assign val (op lookup-variable-value) (reg exp) (reg env)) ;val: factorial
ev-variable : (goto (reg continue))     ;=> ev-appl-did-operator
ev-appl-did-operator : (restore unev)   ;stack: global print-result
ev-appl-did-operator : (restore env)    ;stack: print-result
ev-appl-did-operator : (assign argl (op empty-arglist)) ;argl: ()
ev-appl-did-operator : (assign proc (reg val)) ;proc: factorial-label
ev-appl-did-operator : (test (op no-operands?) (reg unev))
ev-appl-did-operator : (branch (label apply-dispatch))
ev-appl-did-operator : (save proc)      ;stack: factorial print-result
ev-appl-operand-loop : (save argl)      ;stack: () factorial print-result
ev-appl-operand-loop : (assign exp (op first-operand) (reg unev))
ev-appl-operand-loop : (test (op last-operand?) (reg unev)) ;=>true
ev-appl-operand-loop : (branch (label ev-appl-last-arg))
ev-appl-last-arg : (assign continue (label ev-appl-accum-last-arg))
ev-appl-last-arg : (goto (label eval-dispatch))
eval-dispatch : (test (op self-evaluating?) (reg exp))
eval-dispatch : (branch (label ev-self-eval))
ev-self-eval : (assign val (reg exp))   ;val: 3
ev-self-eval : (goto (reg continue))
ev-appl-accum-last-arg : (restore argl) ;stack factorial print-result
ev-appl-accum-last-arg : (assign argl (op adjoin-arg) (reg val) (reg argl)) ;argl (3)
ev-appl-accum-last-arg : (restore proc) ;stack print-result
ev-appl-accum-last-arg : (goto (label apply-dispatch))
apply-dispatch : (test (op primitive-procedure?) (reg proc))
apply-dispatch : (branch (label primitive-apply))
apply-dispatch : (test (op compound-procedure?) (reg proc))
apply-dispatch : (branch (label compound-apply))
apply-dispatch : (test (op compiled-procedure?) (reg proc))
apply-dispatch : (branch (label compiled-apply))
compiled-apply : (restore continue)     ;stack:nil
compiled-apply : (assign val (op compiled-procedure-entry) (reg proc))
compiled-apply : (goto (reg val))
entry250 : (assign env (op compiled-procedure-env) (reg proc))
entry250 : (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) ;n:3
entry250 : (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
entry250 : (assign arg2 (const 1))
entry250 : (assign val (op =) (reg arg1) (reg arg2))
entry250 : (test (op false?) (reg val))
entry250 : (branch (label false-branch253))
false-branch253 : (save continue)       ;stack: print-result
false-branch253 : (assign proc (op get-global-environment))
false-branch253 : (assign proc (op lookup-variable-value) (const factorial) (reg proc))
false-branch253 : (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
false-branch253 : (assign arg2 (const 1))
false-branch253 : (assign val (op -) (reg arg1) (reg arg2)) ;val: 2
false-branch253 : (assign argl (op list) (reg val)) ;argl: (2)
false-branch253 : (test (op primitive-procedure?) (reg proc))
false-branch253 : (branch (label primitive-branch255))
compiled-branch256 : (assign continue (label proc-return258)) ;continue: proc-return258
compiled-branch256 : (assign val (op compiled-procedure-entry) (reg proc))
compiled-branch256 : (goto (reg val))
entry250 : (assign env (op compiled-procedure-env) (reg proc))
entry250 : (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) ;((n) (2))
entry250 : (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env)) ;arg1:2
entry250 : (assign arg2 (const 1))      ;arg2: 1
entry250 : (assign val (op =) (reg arg1) (reg arg2))
entry250 : (test (op false?) (reg val))
entry250 : (branch (label false-branch253))
false-branch253 : (save continue)       ;proc-return258 print-result
false-branch253 : (assign proc (op get-global-environment))
false-branch253 : (assign proc (op lookup-variable-value) (const factorial) (reg proc))
false-branch253 : (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env)) ;arg1 2
false-branch253 : (assign arg2 (const 1)) ;arg2: 1
false-branch253 : (assign val (op -) (reg arg1) (reg arg2)) ;val 1
false-branch253 : (assign argl (op list) (reg val)) ;argl: (1)
false-branch253 : (test (op primitive-procedure?) (reg proc))
false-branch253 : (branch (label primitive-branch255))
compiled-branch256 : (assign continue (label proc-return258)) ;continue: proc-return258
compiled-branch256 : (assign val (op compiled-procedure-entry) (reg proc))
compiled-branch256 : (goto (reg val))
entry250 : (assign env (op compiled-procedure-env) (reg proc))
entry250 : (assign env (op extend-environment) (const (n)) (reg argl) (reg env)) ;((n) (1))
entry250 : (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env)) ;arg1: 1
entry250 : (assign arg2 (const 1))
entry250 : (assign val (op =) (reg arg1) (reg arg2)) ;=>true
entry250 : (test (op false?) (reg val))
entry250 : (branch (label false-branch253))
true-branch252 : (assign val (const 1)) ;val: 1
true-branch252 : (goto (reg continue))
proc-return258 : (assign arg1 (reg val)) ;arg1: 1
proc-return258 : (goto (label after-call257))
after-call257 : (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
after-call257 : (assign val (op *) (reg arg1) (reg arg2))
after-call257 : (restore continue)
after-call257 : (goto (reg continue))
proc-return258 : (assign arg1 (reg val))
proc-return258 : (goto (label after-call257))
after-call257 : (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
after-call257 : (assign val (op *) (reg arg1) (reg arg2))
after-call257 : (restore continue)
after-call257 : (goto (reg continue))
