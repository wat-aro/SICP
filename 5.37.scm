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
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

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

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

;;; 単純な式のコンパイル
(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '() (list target)
                              `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence '(env) (list target)
                              `((assign ,target
                                        (op lookup-variable-value)
                                        (const ,exp)
                                        (reg env))))))

;;; 代入
(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code                 ;valを求めるための命令列が入る．
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code         ;代入する値を求め，valに代入される．
                 ;; valに代入された値をvarに代入する．．
                 (make-instruction-sequence '(env val) (list target)
                                            `((perform (op set-variable-value!)
                                                       (const ,var)
                                                       (reg val)
                                                       (reg env))
                                              (assign ,target (const ok))))))))

;;; 定義
(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence '(env val) (list target)
                                            `((perform (op define-variable!)
                                                       (const ,var)
                                                       (reg val)
                                                       (reg env))
                                              (assign ,target (const ok))))))))

;;; 条件式
(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

;;; 並び
(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving
       '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

;;; lambda式
(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       ;; tack-onでend-withとcompile-lambda-bodyを同じ手続き本体として連結
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

;;; apply
(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        ;; 引数がない場合はarglに'()を代入
        (make-instruction-sequence
         '() '(argl)
         `((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 `((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl
                     (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-cods))))))

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
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
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
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
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
              (assign ,target (reg val))
              (goto (label ,linkage))))))
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

;;; 無駄にsave, restoreを生成する
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (preserving
         (cdr regs)
         (make-instruction-sequence
          (list-union (list first-reg)
                      (registers-needed seq1))
          (list-difference (registers-modified seq1)
                           (list first-reg))
          (append `((save ,first-reg))
                  (statements seq1)
                  `((restore ,first-reg))))
         seq2))))

;;; 本来のpreserving
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

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


;;; save, restoreは使用されない
((env)
 (val)
 ((assign val (op make-compiled-procedure) (label entry34) (reg env))
  (goto (label after-lambda35))
  entry34
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (a b)) (reg argl) (reg env))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (op lookup-variable-value) (const a) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const b) (reg env))
  (assign val (op list) (reg val))
  (assign argl (op append) (reg argl) (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch36))
  compiled-branch37
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch36
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
  after-call38
  after-lambda35
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
  ))

((continue env)                         ;まずcontinueを必要とするようになっている．
 (val)
 ((save continue)                       ;ここでsave continueするから
  (save env)
  (save continue)                       ;ここでさらにsave continueしている．
  (assign val (op make-compiled-procedure) (label entry41) (reg env))
  (restore continue)                    ;ここで復帰．
  (goto (label after-lambda42))         ;ここまでで無駄なsave 3. 無駄なrestore 1
  entry41
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (a b)) (reg argl) (reg env))
  (save continue)                       ;ここでまたsave continue
  (save env)                            ;env
  (save continue)                       ;continue
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (restore continue)                    ;restore c
  (restore env)                         ;restore e
  (restore continue)                    ;restore c
  (save continue)                       ;save c
  (save proc)                           ;save p
  (save env)                            ;save e
  (save continue)                       ;save c
  (assign val (op lookup-variable-value) (const a) (reg env))
  (restore continue)                    ;restore c
  (assign argl (op list) (reg val))
  (restore env)                         ;restore e
  (save argl)                           ;save a
  (save continue)                       ;save c
  (assign val (op lookup-variable-value) (const b) (reg env))
  (restore continue)                    ;restore c
  (restore argl)                        ;restore a
  (assign val (op list) (reg val))
  (assign argl (op append) (reg argl) (reg val))
  (restore proc)                        ;restore p
  (restore continue)                    ;restore c
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch43))
  compiled-branch44
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
  primitive-branch43
  (save continue)                       ;save c
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)                    ;restore c
  (goto (reg continue))
  after-call45
  after-lambda42
  (restore env)                         ;restore e 最初のenv
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
  (restore continue)                    ;最初のcontinue
  ))
