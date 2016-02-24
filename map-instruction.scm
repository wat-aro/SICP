((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry1) (reg env))
  (goto (label after-lambda2))

  entry1
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (proc lst)) (reg argl) (reg env))
  (save continue)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const null?) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch6))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch7))

  compound-branch8
  (assign continue (label after-call9))
  (save continue)
  (goto (reg compapp))

  compiled-branch7
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch6
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;true

  after-call9
  (restore continue)                    ;
  (test (op false?) (reg val))
  (branch (label false-branch4))

  true-branch3
  (assign val (const ()))
  (goto (reg continue))

  false-branch4
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const cons) (reg proc))
  (save continue)
  (save proc)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const map) (reg proc))
  (save proc)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const cdr) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env)) ;lst
  (assign argl (op list) (reg val))     ;(lst)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch18))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch19))

  compound-branch20
  (assign continue (label after-call21))
  (save continue)
  (goto (reg compapp))

  compiled-branch19
  (assign continue (label after-call21))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch18
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl)) ;(cdr lst)

  after-call21
  (assign argl (op list) (reg val)) (((3 4) (5 6)))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env)) ;proc
  (assign argl (op cons) (reg val) (reg argl)) (proc (cdr lst))
  (restore proc)                        ;map
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch23))

  compound-branch24
  (assign continue (label after-call25))
  (save continue)
  (goto (reg compapp))

  compiled-branch23
  (assign continue (label after-call25))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call25
  (assign argl (op list) (reg val))     ;argl: (())
  (save argl)
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env)) ;car
  (save proc)                           ;
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const car) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch10))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch11))

  compound-branch12
  (assign continue (label after-call13))
  (save continue)
  (goto (reg compapp))

  compiled-branch11
  (assign continue (label after-call13))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch10
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call13
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch15))

  compound-branch16
  (assign continue (label after-call17))
  (save continue)
  (goto (reg compapp))

  compiled-branch15
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call17
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch26))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch27))

  compound-branch28
  (save continue)
  (goto (reg compapp))

  compiled-branch27
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  primitive-branch26
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))

  after-call29
  after-if5
  after-lambda2
  (perform (op define-variable!) (const map) (reg val) (reg env))
  (assign val (const ok))
  ))

((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry30) (reg env))
  (goto (label after-lambda31))
  entry30
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (proc lst)) (reg argl) (reg env))
  (save continue)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const null?) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch35))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch36))
  compound-branch37
  (assign continue (label after-call38))
  (save continue)
  (goto (reg compapp))
  compiled-branch36
  (assign continue (label after-call38))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch35
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call38
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch33))
  true-branch32
  (assign val (const ()))
  (goto (reg continue))
  false-branch33
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const cons) (reg proc))
  (save continue)
  (save proc)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const map) (reg proc))
  (save proc)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const cdr) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch47))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch48))
  compound-branch49
  (assign continue (label after-call50))
  (save continue)
  (goto (reg compapp))
  compiled-branch48
  (assign continue (label after-call50))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch47
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call50
  (assign argl (op list) (reg val))
  (assign val (op lexical-address-lookup) (const (0 0)) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch51))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch52))
  compound-branch53
  (assign continue (label after-call54))
  (save continue)
  (goto (reg compapp))
  compiled-branch52
  (assign continue (label after-call54))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch51
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  after-call54
  (assign argl (op list) (reg val))
  (save argl)
  (assign proc (op lexical-address-lookup) (const (0 0)) (reg env))
  (save proc)
  (assign proc (op get-global-environment))
  (assign proc (op lookup-variable-value) (const car) (reg proc))
  (assign val (op lexical-address-lookup) (const (0 1)) (reg env))
  (assign argl (op list) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch39))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch40))
  compound-branch41
  (assign continue (label after-call42))
  (save continue)
  (goto (reg compapp))
  compiled-branch40
  (assign continue (label after-call42))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch39
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call42
  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch43))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch44))
  compound-branch45
  (assign continue (label after-call46))
  (save continue)
  (goto (reg compapp))
  compiled-branch44
  (assign continue (label after-call46))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch43
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call46
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch55))
  (test (op compiled-procedure?) (reg proc))
  (branch (label compiled-branch56))
  compound-branch57
  (save continue)
  (goto (reg compapp))
  compiled-branch56
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch55
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call58
  after-if34
  after-lambda31
  (perform (op define-variable!) (const map) (reg val) (reg env))
  (assign val (const ok))))
