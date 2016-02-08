((env)
 (val)
 (
  ;; 手続きを構成し，本体のコードを飛び越す
  (assign val (op make-compiled-procedure) (label entry18) (reg env))
  (goto (label after-lambda19))
  ;; factorialの呼び出しの開始．
  entry18
  ;; procの環境をenvに代入
  (assign env (op compiled-procedure-env) (reg proc))
  ;; factorialの実引数をfactorialの引数nと対応づけて環境を拡張
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  ;; 内部定義に進む．valを(compiled-procedure entry20 env)の形にする．
  (assign val (op make-compiled-procedure) (label entry20) (reg env))
  (goto (label after-lambda21))

  entry20
  (assign env (op compiled-procedure-env) (reg proc))
  ;; product counterをそれぞれ1に束縛した環境を作る
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  ;; 手続き本体の開始
  (save continue)
  (save env)

  ;; (> counter n)の計算
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))   ;ここに飛ぶ．
  compiled-branch26
  (assign continue (label after-call27))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

  ;; (> counter n)の次
  after-call27                          ;valには(> couner n)の値が入っている
  (restore env)                         ;手続き本体のenvとcontinueを復帰
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch23))

  ;; (> counter n)がtrueの時，productの値をvalに入れて，大本のcontinueへ．
  true-branch22
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))

  ;; (> counter n)がfalseの時
  false-branch23
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  ;; (+ counter 1)を計算するためにcontinue, proc, envを退避
  (save continue)
  (save proc)
  (save env)
  ;; (+ counter 1)の計算開始
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch31))   ;ここへジャンプ
  compiled-branch32
  (assign continue (label after-call33))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  ;; (+ counter 1)を実際に計算
  primitive-branch31
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call33
  (assign argl (op list) (reg val))     ;arglに今計算した値をリストにして代入
  (restore env)                         ;大本の環境の復帰
  (save argl)                           ;(+ counter 1)の結果のリストを退避
  ;; (* counter product)の計算開始
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch28))   ;ここへジャンプ
  compiled-branch29
  (assign continue (label after-call30))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

  ;; (* counter product)を実際に計算
  primitive-branch28
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  after-call30
  (restore argl)                        ;(+ counter 1)の復帰
  (assign argl (op cons) (reg val) (reg argl)) ;arglに((* counter product) (+ counter 1))を代入
  (restore proc)                        ;iterを復帰
  (restore continue)                    ;呼び出し元に返るcontinueを復元
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch34))   ;ジャンプしない
  compiled-branch35
  (assign val (op compiled-procedure-entry) (reg proc)) ;entry20へのラベルをvalに代入
  (goto (reg val))
  primitive-branch34
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call36
  after-if24

  ;; entry18からジャンプ
  after-lambda21
  ;; iterを(compiled-procedure entry20 env)と定義．
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))

  ;; ここから(iter 1 1)の処理．
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  ;; arglは(1)になる
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl)) ;argl => (1 1)
  (test (op primitive-procedure?) (reg proc)) ;=>false
  (branch (label primitive-branch37))

  compiled-branch38
  ;; valにiterに対応付けられてるラベルを代入する
  (assign val (op compiled-procedure-entry) (reg proc))
  ;; entry20へgoto
  (goto (reg val))

  primitive-branch37
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call39
  after-lambda19
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
  ))
