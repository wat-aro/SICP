;; 4.1.1
(define true #t)
(define false #f)

;; eval
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))

;; apply
(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

;; 手続きの引数
;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (cons (eval (first-operand exps) env)
;;             (list-of-values (rest-operands exps) env))))

;; 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; 列
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;; 代入
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

;; 定義
(define (eval-definition exp env)
  (define-variable!
    (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; 4.01
;; 評価順によらず，左から右に評価するlist-of-values
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (first-operand exps) env)))
        (cons first-eval
              (list-of-values (rest-operands exps) env)))))

;; 右から左に評価するlist-of-values
#|
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exp) env)
              first-eval))))
|#
;; 4.1.2
;; 自己評価式 数字と文字列
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 変数
(define (variable? exp) (symbol? exp))

;; クオート式
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

;; リストが指定sれた記号から始まるかどうかを確認する手続き
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; 代入は(set! <var> <value>)の形
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; 定義は(define <var> <value>)の形か
;; (define (<var> <parameter 1> ... <parameter n>)
;;   (<body>)))のの形
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;;仮引数
                   (cddr exp)))) ;;本体

;; lambda式は，記号lambdaから始まるリスト
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; 条件式
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

;; ifのコンストラクタ
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;; コンタストラクタsequence->exp
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; 派生式
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp) (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;; else 説は無い
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; 4.02
;; 本文のeval
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))

;; ;; evalのcondの順番を変えてapplyをassignmentより前にしようとしている
;; ;; a
;; (define x 3)
;; ;; (application? exp)は(pair? exp)なので(define x 3)もapplyされる．
;; ;; applyの節はすべてのリストにマッチするので最後でないといけない．

;; ;; b
;; ;; 手続き適用がcallで始まるように構文を変える．
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))

;; (define (application? exp) (tagged-list? exp 'call))
;; (define (operator exp) (cadr exp))
;; (define (operands exp) (csdr exp))
;; |#

;; 4.03
;; evalをデータ主導スタイルに書き換える．
;; 本文で定義されたeval
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))

;; opを持つexpと持たないexpで分ける．
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         (let ((op (get 'eval (operator exp)))) ;;opが見付からなければfalseが束縛
;;           (cond (op
;;                  (op (operands exp) env))
;;                 ((application? exp)
;;                  (apply (eval (operator exp) env)
;;                         (list-of-values (operands exp) env)))
;;                 (else
;;                  (error "Unknown expression type: EVAL" exp))))))


;; (define (make-table)
;;   (let ((local-table (list '*table*)))
;;     (define (lookup key-1 key-2)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (cdr record)
;;                   false))
;;             false)))
;;     (define (insert! key-1 key-2 value)
;;       (let ((subtable (assoc key-1 (cdr local-table))))
;;         (if subtable
;;             (let ((record (assoc key-2 (cdr subtable))))
;;               (if record
;;                   (set-cdr! record value)
;;                   (set-cdr! subtable
;;                             (cons (cons key-2 value)
;;                                   (cdr subtable)))))
;;             (set-cdr! local-table
;;                       (cons (list key-1
;;                                   (cons key-2 value))
;;                             (cdr local-table)))))
;;       'ok)
;;     (define (dispatch m)
;;       (cond ((eq? m 'lookup-proc) lookup)
;;             ((eq? m 'insert-proc!) insert!)
;;             (else (error "Unknown operation -- TABLE" m))))
;;     dispatch))

;; (define operation-table (make-table))
;; (define get (operation-table 'lookup-proc))
;; (define put (operation-table 'insert-proc!))

;; (define (install-eval-package)
;;   ;; クオート式
;;   (define (text-of-quotation exp) (cadr exp))
;;   ;; 代入
;;   (define (eval-assignment exp env)
;;     (set-variable-value! (assignment-variable exp)
;;                          (eval (assignment-value exp) env)
;;                          env)
;;     'ok)
;;   ;; 定義
;;   (define (eval-definition exp env)
;;     (define-variable! (definition-variable exp)
;;       (eval (definition-value exp) env)
;;       env)
;;     'ok)
;;   ;; 条件式
;;   (define (eval-if exp env)
;;     (if (true? (eval (if-predicate exp) env))
;;         (eval (if-consequent exp) env)
;;         (eval (if-alternative exp) env)))
;;   ;; lambda
;;   (define (lambda-parameters exp) (cadr exp))
;;   (define (lambda-body exp) (cddr exp))
;;   ;; 列
;;   (define (eval-sequence exps env)
;;     (cond ((last-exp? exps)
;;            (eval (first-exp exps) env))
;;           (else
;;            (eval (first-exp exps) env)
;;            (eval-sequence (rest-exps exps) env))))
;;   (define (begin-actions exp) (cdr exp))
;;   (define (cond->if exp) (expand-clauses (cond-clauses exp)))
;;   (define (expand-clauses clauses)
;;     (if (null? clauses)
;;         'false ;; else 説は無い
;;         (let ((first (car clauses))
;;               (rest (cdr clauses)))
;;           (if (cond-else-clause? first)
;;               (if (null? rest)
;;                   (sequence->exp (cond-actions first))
;;                   (error "ELSE clause isn't last: COND->IF"
;;                          clauses))
;;               (make-if (cond-predicate first)
;;                        (sequence->exp (cond-actions first))
;;                        (expand-clauses rest))))))
;;   (put 'eval 'quote text-of-quotation)
;;   (put 'eval 'set! eval-assignment)
;;   (put 'eval 'define eval-definition)v
;;   (put 'eval 'if eval-if)
;;   (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
;;                                                        (lambda-body exp)
;;                                                        env)))
;;   (put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
;;   (put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
;;   'done)

;; 4.04
(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-and clauses env)
  (cond ((null? clauses) 'true)
        ((last-exp? clauses) (eval (first-exp clauses) env))
        ((true? (eval (first-exp clauses) env)) (eval-and (rest-exps clauses)))
        (else 'false)))

(define (eval-or clauses env)
  (if (null? exp)
      'false
      (let ((first-clause (eval (first-exp clauses) env)))
        (if (true? first-clause)
            first-clause
            (eval-or (rest-exps clauses) env)))))

;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((and? exp) (eval-and (and-clauses exp) env))
;;         ((or? exp) (eval-or (or-clauses exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))

;; 派生式としてのandとor
(define (and->if exp) (expand-and-clause (and-clauses exp)))
(define (expand-and-clause clauses)
  (if (null? clauses)
      'true
      (if (lst-exp? clauses)
          (first-exp clauses) ;;最後の式の値を返す.
          (make-if (first-exp clauses)
                   (expand-and-clause (rest-exps clauses))
                   'false))))

(define (or->if exp) (expand-or-clause (or-clauses exp)))
(define (expand-or-clause clauses)
  (if (null? clauses)
      'false
      (let ((first (first-exp clauses)))
        (make-if first
                 first
                 (expand-or-clause (rest-exps clauses))))))

;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((and? exp) (eval (and->if exp) env))
;;         ((or? exp) (eval (or->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))



;; 4.5
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ;; else 説は無い
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (let ((predicate (cond-predicate first))
                  (action (cond-action first)))
              (make-if (cond-predicate first)
                       (if (eq? (car action) '=>)
                           (list ((cadr action) predicate))
                           (sequence->exp action))
                       (expand-clauses rest)))))))


;; 4.6
;; (let ((a 1) (b 2) (c 3))
;;   (+ a b c))

;; ((lambda (a b c)
;;    (+ a b c)) 1 2 3)
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-parameters exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-variables exp)
  (map car (let-parameters exp)))

(define (let-expressions exp)
  (map cadr (let-parameters exp)))

;; (lambda-exp expr)の形のリストにしてevalに渡す
(define (let->combination exp)
  (if (null? exp)
      '()
      (cons (make-lambda (let-variables exp) (let-body xp))
            (let-expressions exp))))

;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;         ((variable? exp) (lookup-variable-value exp env))
;;         ((quoted? exp) (text-of-quotation exp))
;;         ((assignment? exp) (eval-assignment exp env))
;;         ((definition? exp) (eval-definition exp env))
;;         ((if? exp) (eval-if exp env))
;;         ((lambda? exp) (make-procedure (lambda-parameters exp)
;;                                        (lambda-body exp)
;;                                        env))
;;         ((let? exp) (eval (let->combination exp) env)) ;;letを追加
;;         ((begin? exp)
;;          (eval-sequence (begin-actions exp) env))
;;         ((cond? exp) (eval (cond->if exp) env))
;;         ((and? exp) (eval (and->if exp) env))
;;         ((or? exp) (eval (or->if exp) env))
;;         ((application? exp)
;;          (apply (eval (operator exp) env)
;;                 (list-of-values (operands exp) env)))
;;         (else
;;          (error "Unknown expression type: EVAL" exp))))

;; 4.07
;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))

;; (let ((x 3))
;;   (let ((y (+ x 2)))
;;     (let ((z (+ x y 5)))
;;       (* x z))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-parameters exp) (cadr exp))
(define (let*-body exp) (cddr exp))

(define (make-let parameters bodys)
  (append (list 'let parameters) bodys))

(define (make-let* parameters bodys)
  (append (list 'let* parameters) bodys))

(define (let*->nested-lets exp)
  (cond ((null? (let*-parameters exp)) (let*-body exp))
        ((null? (cdr (let*-parameters exp)))
         (make-let (list (car (let*-parameters exp))) (let*-body exp)))
        (else
         (make-let (list (car (let*-parameters exp)))
                   (list (let*->nested-lets
                          (make-let* (cdr (let*-parameters exp)) (let*-body exp))))))))

(define (eval exp env)
  (cond ((self-evaluating? #?=exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env)) ;;let*を追加
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        (#?=(application? exp)
         (my-apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

;; 4.08
(define (let-parameters exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-variables exp)
  (map car (let-parameters exp)))

(define (let-expressions exp)
  (map cadr (let-parameters exp)))

;; (let var bindings body)のiterにあたるところ．
(define (named-let-name exp) (cadr exp))
(define (named-let-body exp) (cadddr exp))
(define (named-let-bindings exp) (caddr exp))

;; bindingsは((variable expression) (variable2 expression2) ...)の形
(define (named-let-variable exp)
  (map car (named-let-bindings exp)))
(define (named-let-expressions exp)
  (map cadr (named-let-bindings exp)))

(define (make-parameters variable expression)
  (list (list variable expresstion)))

(define (named-let? exp)
  (symbol? (named-let-name exp)))

;; (define (let->combination exp)
;;   (cond ((named-let? exp) ;;named-letはletに直してもう一度let->combinationにかける
;;          (let->combination
;;           (make-let
;;            (make-parameters (named-let-name exp)
;;                             (make-lambda (named-let-variable exp)
;;                                          (named-let-body exp)))
;;            (cons (named-let-name exp) (named-let-expressions exp)))))
;;         ((null? exp) '())
;;         (else (cons (make-lambda (let-variables exp) (let-body xp))
;;                     (let-expressions exp)))))

(define (let->combination exp)
  (cond ((named-let? exp)
         (make-begin (list
                      (make-definition (named-let-name exp)
                                       (make-lambda (named-let-variable exp)
                                                    (named-let-body exp)))
                      (named-let-expressions exp)))
         ((named-let-name exp) (named-let-expressions exp)))
        ((null? exp) '())
        (else (cons (make-lambda (let-variables exp) (let-body exp))
                    (let-expressions exp)))))

;; 4.09
;; iteratorの実装
;; whileの使用例
;; (while (< i 10)
;;   (begin (display i)
;;          (newline)
;;          (set! i (+ i 1))))

;; (let while ()
;;   (if (< i 10)
;;       (begin
;;         (begin (display i)
;;                (newline)
;;                (set! i (+ i 1)))
;;         (while))))

;; 破壊的です．
;; (define (while? exp) (tagged-list? exp 'while))
;; (define (while-predicate exp) (cadr exp))
;; (define (while-body exp) (caddr exp))

;; (define (while->let exp)
;;   (make-named-let 'my-while '()
;;                   (make-if (while-predicate exp)
;;                            (meke-begin
;;                             (list (while-body exp)
;;                                   '(my-while)))
;;                            '())))

;; 4.10
;; 後置式にする
;; 全部はめんどうなのでquoteだけ．
;; リストの最後の項か尋ねるlast?
;; 空リストは#fを返す．
;; (define (last? lst)
;;   (if (null? lst)
;;       #f
;;       (null? (cdr lst))))

;; ;; リストの最後の項を取る選択子last
;; (define (last lst)
;;   (if (last? lst)
;;       lst
;;       (lst (cdr lst))))

;; ;; cdrの逆で最後の項を取り除いたリストを返す
;; (define (rid-last lst)
;;   (let iter ((lst lst)
;;              (result '()))
;;     (cond ((null? lst) (error "pair required, but got" lst))
;;           ((last? lst) (reverse result))
;;           (else (iter (cdr lst) (cons (car lst) result))))))

;; ;; クオート式
;; (define (quoted? exp) (tagged-list? exp 'quote))
;; (define (text-of-quotation exp) (rid-last exp))

;; ;; リストが指定sれた記号から始まるかどうかを確認する手続き
;; (define (tagged-list? exp tag)
;;   (if (pair? exp)
;;       (eq? (last exp) tag)
;;       false))


;; 4.1.3
;; 術語のテスト
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

;; 手続きの表現
;; (define (make-procedure parameters body env)
;;   (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; 環境に対する演算
;; 環境はフレームのリストとして表現
;; リストのcdrが外側の環境
(define (enclosing-environment env) (cdr env))
(define (first-frame env)(car env))
(define the-empty-environment '())

;; フレームはリストのペアとして表現
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; 環境を拡張するには，変数のリストと値のリストからなるフレームを作り，環境に追加．
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

;; 一つ目のフレームを探し，見つかれば値を返す．見つからなければ次のフレームへ．
;; 環境が空になればエラーを返す．
;; (define (lookup-variable-value var env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars)) (car vals))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable" var)
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame)))))
;;   (env-loop env))

;; lookupと同じようにvarを探し，見つかればset-car!で書き換え，見つからなければエラーを返す
;; (define (set-variable-value! var val env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars)) (set-car! vals val))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable: SET!" var)
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame)))))
;;   (env-loop env))

;; (define (define-variable! var val env)
;;   (let ((frame (first-frame env)))
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (add-binding-to-frame! var val frame))
;;             ((eq? var (car vars)) (set-car! vals val))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (scan (frame-variables frame) (frame-values frame))))

;; 4.11
;; フレームを束縛のリストとして表現
;; (define (make-frame variables values)
;;   (map cons variables values))

;; (define (first-binding frame) (car frame))
;; (define (rest-bindings frame) (cdr frame))

;; (define (binding-variable binding) (car binding))
;; (define (binding-value binding) (cdr binding))
;; (define (make-binding var val) (cons var val))


;; (define (add-binding-to-frame! var val fram)
;;   (set! frame (cons (make-binding var val) frame)))

;; ;; 変更無し
;; (define (extend-environment vars vals base-env)
;;   (if (= (length vars) (length vals))
;;       (cons (make-frame vars vals) base-env)
;;       (if (< (length vars) (length vals))
;;           (error "Too many arguments supplied" vars vals)
;;           (error "Too few arguments supplied" vars vals))))

;; ;; 変更無し
;; (define (lookup-variable-value var env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars)) (car vals))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable" var)
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame)))))
;;   (env-loop env))

;; ;; frame-variablesとframe-valuesを作ればset-variable-value!とdefine-variable!は変更なし
;; (define (frame-variables frame) (map car frame))
;; (define (frame-values frame) (map cdr frame))
;; (define (set-variable-value! var val env)
;;   (define (env-loop env)
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (env-loop (enclosing-environment env)))
;;             ((eq? var (car vars)) (set-car! vals val))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (if (eq? env the-empty-environment)
;;         (error "Unbound variable: SET!" var)
;;         (let ((frame (first-frame env)))
;;           (scan (frame-variables frame)
;;                 (frame-values frame)))))
;;   (env-loop env))

;; (define (define-variable! var val env)
;;   (let ((frame (first-frame env)))
;;     (define (scan vars vals)
;;       (cond ((null? vars)
;;              (add-binding-to-frame! var val frame))
;;             ((eq? var (car vars)) (set-car! vals val))
;;             (else (scan (cdr vars) (cdr vals)))))
;;     (scan (frame-variables frame) (frame-values frame))))

;; ;; 作らない場合は
;; (define (set-variable-value var val env)
;;   (define (env-loop env)
;;     (let ((target (assq var (first-frame env))))
;;       (if target
;;           (set-cdr! target val)
;;           (env-loop (enclosing-environment env)))))
;;   (env-loop env))

;; (define (define-variable! var val env)
;;   (let* ((frame (first-frame env))
;;          (target (assq var frame)))
;;     (if target
;;         (set-cdr! target val)
;;         (add-binding-to-frame! var val frame))))

;; 4.12
;; scanとenv-loopを抜き出す．
;; 見つかった時の手続きをprocで渡す．
(define (scan var vars vals proc)
  (cond ((null? vars) #f)
        ((eq? var (car vars)) (proc var vars vals))
        (else (scan var (cdr vars) (cdr vals) proc))))

(define (env-loop var env proc)
  (if (eq? env the-empty-environment)
      #f
      (let ((frame (first-frame env)))
        (let ((val (scan var
                         (frame-variables frame)
                         (frame-values frame)
                         proc)))
          (or val (env-loop var (enclosing-environment env) proc))))))


;; (define (lookup-variable-value var env)
;;   (let ((target (env-loop var env (lambda (var vars vals) vals))))
;;     (if target
;;         (car target)
;;         (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((target (env-loop var env (lambda (var vars vals)
                                    (set-car! vals val)))))
    (if target
        target
        (error "Unbound variable: SET!" var))))

;; 見つかればtargetにvalsが束縛される．
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((target (scan var (frame-variables frame) (frame-values frame)
                        (lambda (var vars vals) vals))))
      (if target
          (set-car! target val)
          (add-binding-to-frame! var val frame)))))

;; 4.13
;; (define (scan var vars vals proc)
;;   (cond ((null? vars) #f)
;;         ((eq? var (car vars)) (proc var vars vals))
;;         (else (scan var (cdr vars) (cdr vals) proc))))

;; 束縛された変数を解放するmake-unbound!
;; first-frameだけでに限定しないと
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (let ((target (scan var (frame-variables frame) (frame-values frame) (lambda (var vars vals)
                                                                           (set! vars (cdr vars))
                                                                           (set! vals (cdr vals))
                                                                           'ok))))
      (if target
          target
          (error "Unbound variable" var)))))

;; 4.1.4
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '- -)
        (list '= =)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; 評価器をプログラムとして実行する
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define apply-in-underlying-scheme apply) ;; apply


(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-EVAL input:")
(define output-prompt ";;; M-EVAL value")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


;; 4.14
;; M-EVAL中の手続きは先頭に'procedureがついたリスト．
;; 基層のLispのmapを使うとただのリストとして受け取ってしまうためにうまくいかない．
;; (define primitive-procedures
;;   (list (list 'car car)
;;         (list 'cdr cdr)
;;         (list 'cons cons)
;;         (list 'null? null?)))

;; (define (primitive-procedure-names)
;;   (map car primitive-procedures))

;; (define (primitive-procedure-objects)
;;   (map (lambda (proc) (list 'primitive (cadr proc)))
;;        primitive-procedures))

;; (define (setup-environment)
;;   (let ((initial-env
;;          (extend-environment (primitive-procedure-names)
;;                              (primitive-procedure-objects)
;;                              the-empty-environment)))
;;     (define-variable! 'true true initial-env)
;;     (define-variable! 'false false initial-env)
;;     initial-env))

;; (define the-global-environment (setup-environment))

;; 4.15
;; 引数の手続きpとオブジェクトaについて，式(p a)停止するかどうかを正確に判断するような手続きhalts?
;; (define (run-forever) (run-forever))
;;
;; (define (try p)
;;   (if (halts? p p)
;;       (run-forever)
;;       'halted))

;; (try try)

;; まずこれが停止すると仮定する．すると(halts? try try)はtrueを返し，then節(run-forever)を実行するので停止しない．
;; 次にこれが停止しないと仮定する．(halts? try try)がfalseなのでelse節'haltsが実行されるので停止する．
;; 矛盾するのでhalts?は定義できない．

;; 4.1.6 内部定義
;;'#0=((procedure (x) ((cons x 'a)) (((app false true car cdr cons null?) . #0#))) #f #t (primitive #<subr car>) (primitive #<subr cdr>) (primitive #<subr cons>) (primitive #<subr null?>))

;; 4.16
;; a
(define (lookup-variable-value var env)
  (let ((target (env-loop var env (lambda (var vars vals) vals))))
    (cond ((eq? target '*unassigned*) (error "Unassigned variable" var))
          (target (car target))
          (else (error "Unbound variable" var)))))

;; b
(define (scan-out-defines proc-body)
  ;; 選択子
  (define (def-list def-body-list) (car def-body-list))
  (define (body-list def-body-list) (cdr def-body-list))
  ;; lambda式の本体を受け取って，内部でdefineを使ってる式と使ってない式のリストを返す
  (define (split-def-body proc-body-list)
    (let iter ((proc-body-list proc-body-list)
               (def '())
               (body '()))
      (cond ((null? proc-body-list) (cons (reverse def) (reverse body)))
            ((definition? (car proc-body-list))
             (iter (cdr proc-body-list) (cons (car proc-body-list) def) body))
            (else (iter (cdr proc-body-list) def (cons (car proc-body-list) body))))))
  ;; 本体
  (let ((def-body-list (split-def-body proc-body)))
    (if (null? (def-list def-body-list))
        proc-body
        (list (make-let (map (lambda (x) (list (definition-variable x) ''*unassigned*))
                             (def-list def-body-list))
                        (append (map (lambda (x) (list 'set!
                                                       (definition-variable x)
                                                       (definition-value x)))
                                     (def-list def-body-list))
                                (body-list def-body-list)))))))

;; c
;; どちらに組み込んだでも同じが，procedure-bodyは二箇所で呼ばれているので一箇所でしか呼ばれていないmake-procedureに組み込んだ方が良い．
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


;; 4.17
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;;仮引数
                   (cddr exp)))) ;;本体

;; lambda式は，記号lambdaから始まるリスト
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (scan-out-defines proc-body)
  ;; 選択子
  (define (def-list def-body-list) (car def-body-list))
  (define (body-list def-body-list) (cdr def-body-list))
  ;; lambda式の本体を受け取って，内部でdefineを使ってる式と使ってない式のリストを返す
  (define (split-def-body proc-body-list)
    (let iter ((proc-body-list proc-body-list)
               (def '())
               (body '()))
      (cond ((null? proc-body-list) (cons (reverse def) (reverse body)))
            ((definition? (car proc-body-list))
             (iter (cdr proc-body-list) (cons (car proc-body-list) def) body))
            (else (iter (cdr proc-body-list) def (cons (car proc-body-list) body))))))
  ;; 本体
  (let ((def-body-list (split-def-body proc-body)))
    (if (null? (def-list def-body-list))
        proc-body
        (cons (make-lambda (map (lambda (x) (definition-variable x))
                                (def-list def-body-list))
                           (body-list def-body-list))
              (map definition-value (def-list def-body-list))))))


(define (eval exp env)
  (cond ((self-evaluating? #?=exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        (#?=(lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env)) ;;let*を追加
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((application? exp)
         (my-apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? #?=procedure)
         (apply-primitive-procedure #?=procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

((lambda (x)
   ((lambda (f g)
      (f (g 1 x)))
    (lambda (x) (cons 'a x))
    (lambda (a b) (cons a b))))
 5)

(eval-sequence (procedure-body '(procedure (x)
                                           ((lambda (even? odd?) (cons x x) (even? x))
                                            (lambda (n) (if (= n 0) true (odd? (- n 1))))
                                            (lambda (n) (if (= n 0) false (even? (- n 1))))) #0#))
               (extend-environment
                (procedure-parame '(procedure (x)
                                              ((lambda (even? odd?) (cons x x) (even? x))
                                               (lambda (n) (if (= n 0) true (odd? (- n 1))))
                                               (lambda (n) (if (= n 0) false (even? (- n 1))))) #0#))
                '(5)
                (procedure-environment (eval 'f the-global-environment))))

;; 4.22
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp) (analyze (let->combination exp)))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
      (lambda (env) exp)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env)
      (make-procedure vars bproc env))))

;; loopからsequentiallyの流れ．
;; (lambda (env) (p1 env) (p2 env))
;; (lambda (ENV) ((lambda (env) (p1 env) (p2 env)) ENV) (p3 env))
;; (lambda (ENV) (p1 ENV) (p2 ENV) (p3 ENV))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env)
      (proc1 env)
      (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE")
        (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (pproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error "Unknown procedure type -- EXECUTE-APPLICATION" proc))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
