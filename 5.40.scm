(load "./eval.scm")

;;; make-branchのための手続き
(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

;;; make-compileに必要な機械演算
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define all-regs '(env proc val argl continue))

;; (define (compile exp target linkage)
;;   (cond ((self-evaluating? exp)
;;          (compile-self-evaluating exp target linkage))
;;         ((quoted? exp) (compile-quoted exp target linkage))
;;         ((variable? exp)
;;          (compile-variable exp target linkage))
;;         ((assignment? exp)
;;          (compile-assignment exp target linkage))
;;         ((definition? exp)
;;          (compile-definition exp target linkage))
;;         ((if? exp) (compile-if exp target linkage))
;;         ((lambda? exp) (compile-lambda exp target linkage))
;;         ((begin? exp)
;;          (compile-sequence (begin-actions exp)
;;                            target linkage))
;;         ((cond? exp) (compile (cond->if exp) target linkage))
;;         ((application? exp)
;;          (compile-application exp target linkage))
;;         (else
;;          (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;;; 接続コードの翻訳
(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

;;; 命令の最後に次の計算の行き先を入れる．
;;; preservingがあるのでlinkageがreturnでinstruction-sequenceがcontinueを変更しても
;;; save, restoreされるので問題ない
(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

;;; 単純な式のコンパイル
;;; targetにexpを代入して次の計算への命令を作る
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,exp))))))

;;; targetに(cadr exp)を代入して次の計算への命令を作る
(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,(text-of-quotation exp)))))))

;;; variableを環境から探してきて，見つかった値をtargetに代入して，次の計算への命令を足して返す
(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '(env) (list target)
                              `((assign ,target
                                        (op lookup-variable-value)
                                        (const ,exp)
                                        (reg env))))))

;;; 代入
(define (compile-assignment exp target linkage ct-env)
  (let ((var (assignment-variable exp))
        (get-value-code                 ;valを求めるための命令．
         (compile (assignment-value exp) 'val 'next ct-env)))
    (end-with-linkage
     linkage
     (preserving '(env)                 ;valを求める間に環境が変わると困る
                 get-value-code         ;代入する値を求め，valに代入される．seq1
                 ;; valに代入された値をvarに代入する．seq2
                 (make-instruction-sequence
                  ;;  ;代入するので元々の環境と代入する値を必要とする．
                  '(env val)
                  ;; targetに'okを入れて返すのでtargetは変更する
                  (list target)
                  `((perform (op set-variable-value!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))

;;; 定義
(define (compile-definition exp target linkage ct-env)
  (let ((var (definition-variable exp)) ;糖衣構文(f x)の場合でもfがvarに束縛される
        (get-value-code                 ;varに束縛する値を求める命令
         (compile (definition-value exp) 'val 'next ct-env)))
    (end-with-linkage
     linkage
     (preserving '(env)                 ;valを求める間に環境が変わると困る
                 get-value-code
                 (make-instruction-sequence
                  ;;定義する元々の環境とget-value-codeで求めた値の入っているvalが必要
                  '(env val)
                  (list target)         ;targetにokを入れて返す
                  `((perform (op define-variable!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))

;;; 条件式
;;; ifはtestがtrueならfalseに飛ぶ．
;;; そのためlinkageがnextの場合，そのままだとtrueの後にfalseにいってしまう
;;; falseを飛ばすためにtrueの後はafter-ifに飛ぶように
;;; nextの場合はconsequenct-linkageにafter-ifを入れる．
(define (compile-if exp target linkage ct-env ct-env)
  ;; make-branchで書くラベルにIDをつける
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage           ;nextならafter-ifが入る
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next ct-env)) ;術後を生成する
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage ct-env)) ;consequenct節の命令の生成
            (a-code
             (compile (if-alternative exp) target linkage ct-env))) ;alterenative節の命令の生成
        (preserving '(env continue)     ;環境とcontinueは保護
                    p-code
                    (append-instruction-sequences ;任意の数の式をつながりのある式として連結する
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     ;; prallelで逐次実行でなくどちらかだけが実行される命令を作る
                     ;; これはどちらが選ばれるか実行時までわからないので
                     ;; neededとmodifiedの和集合をとる．
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

;;; 並び
;;; beginやlambdaのbodyで使う
(define (compile-sequence seq target linkage ct-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage ct-env)
      (preserving
       '(env continue)                  ;環境と継続は保護
       (compile (first-exp seq) target 'next ct-env) ;そのまま次の命令を実行するのでnext
       (compile-sequence (rest-exps seq) target linkage ct-env)))) ;再帰的に命令列を作る

;;; lambda式
;;; target(val)にコンパイルした式のラベルを束縛してlambda-linkageにジャンプ
;;; 実際に式を呼び出すときにcompile-lambda-bodyで作るラベルにジャンプし，処理をする
(define (compile-lambda exp target linkage ct-env)
  (let ((proc-entry (make-label 'entry)) ;コンパイルされた式はこのentry-idのラベルで処理される
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       ;; tack-onでend-with-linkageにcompile-lambda-bodyを連結．
       ;; neededとmodifiedはend-with-linkageのほうを使う
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry ct-env))
       after-lambda))))

;;; コンパイルした手続きが実際に処理をするラベルの中身を作る
(define (compile-lambda-body exp proc-entry ct-env)
  (let ((formals (lambda-parameters exp))) ;lambdaの引数はformalsに束縛
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      ;; 実際の処理をするラベル
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env                     ;ここで仮引数と実引数で環境を拡張
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     ;; lambdaのbodyは式が複数のことがあるのでcompile-sequence
     ;; 呼び出し元に値を返さないと行けないのでlinkageはreturn
     (compile-sequence (lambda-body exp) 'val 'return (cons formals ct-env)))))

;;; apply
(define (compile-application exp target linkage ct-env)
  (let (
        ;; operatorをコンパイルしたら次はoperandの評価をしなければいけないのでnext
        (proc-code (compile (operator exp) 'proc 'next ct-env))
        ;; operandは複数なのでそれぞれcompileしてリストにして保持
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next ct-env))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code                          ;最初にoperatorを確定させる
     (preserving
      '(proc continue)
      (construct-arglist operand-codes) ;operandを評価してarglに代入するための命令の生成
      (compile-procedure-call target linkage))))) ;

;;; compile-applicationでoperand-codesはコンパイル済みなのでそれをarglに入れるための命令を生成
(define (construct-arglist operand-codes)
  ;; reverseして連結していくので右から左に評価することになる
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        ;; 引数がない場合はarglに'()を代入
        (make-instruction-sequence
         '() '(argl)
         `((assign argl (const ()))))
        (let ((code-to-get-last-arg     ;最後のoperandが生成する命令
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)         ;arglの初期化が必要なのでこれだけ特別に処理
                 `((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg      ;cdrがnullなら最後のoperand
              ;; まだoperandが残っていればこちら
              (preserving
               '(env)                   ;環境は保持
               code-to-get-last-arg     ;引数の最後（reverseしているので先頭）からつなげる.
               (code-to-get-rest-args
                (cdr operand-codes))))))))

;;; last-arg以外はここで処理する
;;; operand-codesはコンパイル済み
;;; arglには既に最後の引数が代入されているのでそこに先頭(reverseしてるので後ろ)から代入していく
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg              ;先頭
         (preserving
          '(argl)
          (car operand-codes)           ;valに先頭の要素のコンパイル結果を代入する命令
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl               ;valに入った(car operand)の値をarglに代入
                     (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-cods))))))

;;; operator, operandsを評価する命令を作った後に呼ばれる
;;; この時点でprocにはoperatorのシンボル, arglにはoperandsが入っている
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       ;; compiled-branchかprimitive-branchのどちらかだけが実行されるのでparallel
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         ;; ここでtargetとlinkageに合わせた命令を生成
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage
          linkage
          (make-instruction-sequence
           '(proc argl) (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;;; 手続きの採用
(define (compile-proc-appl target linkage)
  (cond (
         ;; linkageがreturnでなければlinkageにはいったlabelが値を返す場所
         (and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage)) ;計算した値をvalに入れたらこのlinkageにジャンプ
            (assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ;; targetがvalでないのでproc-returnでtargetにvalを代入しないといけない
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val)) ;targetがvalでないので，ここでtargetにvalを代入
              (goto (label ,linkage))))))
        ;; targetがvalでreturnなら計算の後，continueに行けばいいので余計な処理はない
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          `((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

;;; 命令列の組み合わせ
(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;;; neededとmodifiedをうまく合成して新しい命令列を作る
;;; これは人つながりの命令にする．
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     ;; needed
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2) ;seq1で変更してseq2がそれを必要とする
                                  (registers-modified seq1))) ;ならseq1の時点では必要ない
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)  ;nullじゃなければこっち．
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

;;; 集合演算
(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

;;; regsの中にseq1で変更してseq2でしようするレジスタがあれば
;;; seq1の前後でsave, restoreする命令を作る．
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))     ;first-regが
        (if (and (needs-register? seq2 first-reg) ;seq2に必要なレジスタで
                 (modifies-register? seq1 first-reg)) ;seq1が変更するレジスタなら
            (preserving
             (cdr regs)
             (make-instruction-sequence
              ;; needs ここでsaveするのでfirst-regが必要になるのでlist-union
              (list-union (list first-reg)
                          (registers-needed seq1))
              ;; modify saveしてのseq2の前にrestoreするのでseq2から見ればfirst-reg変更無し
              (list-difference (registers-modified seq1)
                               (list first-reg))
              ;; statements 条件を満たすfirst-regの場合はseq1をsaveとrestoreで挟む
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

;;; seqとbodyとbody-seqをつなげる．neededとmodifiedはseqのまま
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

;;; neededとmodifiedは和集合を取る．
;;; ifのconsequentとalternative, や
;;; 手続きのcompiled, primitiveの違いのようにどちらかだけが実行されるようなラベルを作るときに使う
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


;; ;;; 5.38
(define (open-code? exp)
  (memq (car exp) '(= * - +)))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((open-code? exp)               ;open-code?でdispatch
         (compile-open-code exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (spread-arguments operand)      ;それぞれコンパイルしてリストにして返す
  (let ((co-arg1 (compile (car operand) 'arg1 'next))
        (co-arg2 (compile (cadr operand) 'arg2 'next)))
    (list co-arg1 co-arg2)))

(define (compile-open-code exp target linkage)
  (if (= (length exp) 3)
      (let ((proc (operator exp))
            (args (spread-arguments (operands exp))))
        (end-with-linkage linkage
                          (append-instruction-sequences
                           (car args)
                           ;; co-arg2がopen-code式だった場合にarg1が上書きされるので退避させる．
                           (preserving
                            '(arg1)
                            (cadr args)
                            (make-instruction-sequence
                             '(arg1 arg2)
                             (list target)
                             `((assign ,target (op ,proc) (reg arg1) (reg arg2))))))))
      (error "require 2 operand" exp)))


;;; 5.38-d
(define (compile-open-code exp target linkage)
  (cond ((= (length exp) 3)
         (compile-open-code-operand exp target linkage))
        ((or (tagged-list? exp '+)
             (tagged-list? exp '*))
         (compile-open-code-operand-2 (operator exp) (operands exp) target linkage))
        (error "invalid application: " exp)))

(define (compile-open-code-operand exp target linkage)
  (let ((proc (operator exp))
        (args (spread-arguments (operands exp))))
    (end-with-linkage linkage
                      (append-instruction-sequences
                       (car args)
                       ;; co-arg2がopen-code式だった場合にarg1が上書きされるので退避させる．
                       (preserving
                        '(arg1)
                        (cadr args)
                        (make-instruction-sequence
                         '(arg1 arg2)
                         (list target)
                         `((assign ,target (op ,proc) (reg arg1) (reg arg2)))))))))

;;; operandが無くてprocが+なら1を，*なら0をtargetに代入．
;;; operandが一つだけならそのままの値をtargetに入れる．
;;; operandが３つ以上なら
(define (compile-open-code-operand-2 proc operands target linkage)
  (cond ((null? operands)
         (if (eq? proc '+)
             (compile-self-evaluating 0 target linkage) ;+なら0
             (compile-self-evaluating 1 target linkage)))   ;*なら1
        ((null? (cdr operands))
         (end-with-linkage linkage
          (make-instruction-sequence
           '(arg1)
           (list target)
           `((assign ,target (cont ,(car operands)))))))
        (else                           ;引数が３つ以上ならこちらで処理
         (let ((operand (spread-arguments operands)))
           (end-with-linkage
            linkage
            (append-instruction-sequences
             (car operand)
             (compile-open-code-operand-3 proc (cdr operand) target)))))))

;;; ここに渡されるseqはコンパイルされた引数のリスト．
;;; last-seqだとarg1を保護しながら最後の引数をarg2に代入して
;;; 最後にarg1, arg2をprocした結果をvalに代入する．
;;; まだ残っているときはarg1を保護しながら引数をarg2に代入して
;;; arg1とarg2をprocした結果をarg1に代入する
(define (compile-open-code-operand-3 proc seq target)
  (if (last-seq? seq)
      (preserving
       '(arg1)
       (car seq)
       (make-instruction-sequence
        '(arg1 arg2)
        (list target)
        `((assin target (op ,proc) (reg arg1) (reg arg2)))))
      (append-instruction-sequences
       (preserving
        '(arg1)
        (car seq)
        (make-instruction-sequence '(arg1 arg2) '(arg1)
                                   `((assign arg1 (op ,proc) (reg arg1) (reg arg2)))))
       (compile-open-code-operand-3 proc (cdr seq) target))))

;;; operandが0または1以外の時はここでcompileする．
;;; 一つ目だけarg1に代入し，残りはarg2に代入する．
(define (spread-arguments operand)
  (let iter ((operand (cdr operand))
             (result (list (compile (car operand) 'arg1 'next))))
    (if (null? operand)
        (reverse result)
        (iter (cdr operand)
              (cons (compile (car operand) 'arg2 'next) result)))))

(define (last-seq? seq)
  (null? (cdr seq)))

;;; 5.39
;;; 文面アドレスを使って変数の値を探す
(define (lexical-address-lookup lex-add r-env)
  (let ((frame (frame-values (list-ref r-env (car lex-add)))))
    (let ((val (list-ref frame (cdr lex-add))))
      (if (eq? val '*unassigned*)
          (error "*Unassigned* variable")
          val))))

;;; 文面アドレスにある値を変更する
(define (lexical-address-set! lex-add val r-env)
  (let ((frame (frame-values (list-ref r-env (car lex-add)))))
    (let ((target (list-ref frame (cdr lex-add))))
      (set! target val)
      'ok)))

;;; 5.40

