(define true #t)
(define false #f)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((letrec? exp) (eval (letrec->let exp) env)) ;;letrecを追加
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (my-apply (actual-value (operator exp) env)
                   (operands exp)
                   env)
         ;; (my-apply (eval (operator exp) env)
         ;;           (list-of-values (operands exp) env))
         )
        (else
         (error "Unknown expression type --EVAL" exp))))

;; メモ化する評価器
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ;;expをその値で置き換える
           (set-cdr! (cdr obj) '())    ;;必要のなくなったenvを忘れる
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;; メモ化しない評価器
;; (define (force-it obj)
;;   (if (thunk? obj)
;;       (actual-value (thunk-exp obj) (thunk-env obj))
;;       obj))

(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? exp) (tagged-list? exp 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (actual-value exp env)
  (force-it (eval exp env)))
(define (foo bar)
  (bar 'a))

;; apply
(define (my-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              (list-of-delayed-args arguments env) ; changed
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;; apply
;; (define (my-apply procedure arguments)
;;   (cond ((primitive-procedure? procedure)
;;          (apply-primitive-procedure procedure arguments))
;;         ((compound-procedure? procedure)
;;          (eval-sequence
;;           (procedure-body procedure)
;;           (extend-environment
;;            (procedure-parameters procedure)
;;            arguments
;;            (procedure-environment procedure))))
;;         (else
;;          (error "Unknown procedure type -- APPLY" procedure))))


(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

;; (define (list-of-values exps env)
;;   (if (no-operands? exps)
;;       '()
;;       (let ((first (eval (first-operand exps) env)))
;;         (cons first
;;               (list-of-values (rest-operands exps) env)))))

;; 条件式
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; (define (eval-if exp env)
;;   (if (true? (eval (if-predicate exp) env))
;;       (eval (if-consequent exp) env)
;;       (eval (if-alternative exp) env)))

;; 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
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

;; 自己評価式
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 変数
(define (variable? exp) (symbol? exp))

;; クオート
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; 代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;; 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ;;仮パラメタ
                   (cddr exp)))) ;;本体

;; lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;; 任意の合成式
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

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (let ((action (cond-actions first))
                  (predicate (cond-predicate first)))
              (make-if predicate
                       (if (eq? (car action) '=>)
                           (list (cadr action) predicate)
                           (sequence->exp action))
                       (expand-clauses rest)))))))

;; and
(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))

(define (eval-and exp env)
  (let iter ((clauses (and-clauses exp)))
    (if (null? clauses)
        'true
        (let ((first (eval (car clauses) env)))
          (cond ((null? (cdr clauses)) first)
                (first (iter (cdr clauses)))
                (else 'false))))))


;; or
(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-or exp env)
  (let iter ((clauses (or-clauses exp)))
    (if (null? clauses)
        'false
        (let ((first (eval (car clauses) env)))
          (cond ((null? (cdr clauses)) first)
                (first 'true)
                (else (iter (cdr clauses))))))))


;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (cadr exp))
(define (let-variables exp) (map car (let-parameters exp)))
(define (let-expressions exp) (map cadr (let-parameters exp)))
(define (let-bodys exp) (cddr exp))

(define (let->combination exp)
  (if (symbol? (cadr exp)) ;; 2番目の要素がシンボルならnamed-let
      (named-let->define (named-let-func-name exp)
                         (named-let-variables exp)
                         (named-let-expressions exp)
                         (named-let-bodys exp))
      (cons (make-lambda (let-variables exp)
                     (let-bodys exp))
        (let-expressions exp))))

;; let*
(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-parameters exp) (cadr exp))
(define (let*-variables exp) (map car (let*-parameters exp)))
(define (let*-expressions exp) (map cadr (let*-parameters exp)))
(define (let*-body exp) (cddr exp))

(define (make-let parameters bodys)
  (cons 'let (cons parameters bodys)))

(define (let*->nested-lets exp)
  (expand-lets (let-parameters exp) (let-bodys exp)))

(define (expand-lets parameters bodys)
  (cond ((null? parameters) (error "EXPAND-LETS required pair, but " parameters))
        ((null? (cdr parameters))
         (make-let (list (car parameters))
                   bodys))
        (else (make-let (list (car parameters))
                        (list (expand-lets (cdr parameters) bodys))))))


;; named-let
(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-func-name exp) (cadr exp))
(define (named-let-parameters exp) (caddr exp))
(define (named-let-variables exp) (map car (named-let-parameters exp)))
(define (named-let-expressions exp) (map cadr (named-let-parameters exp)))
(define (named-let-bodys exp) (cdddr exp))

(define (make-definition variable value)
  (list 'define variable value))


(define (named-let->define func-name variables expressions bodys)
  (make-begin (list (make-definition func-name (make-lambda variables bodys))
                    (cons func-name expressions))))

;; 術後のテスト
(define (true? x)
  (not (eq? x '#f)))
(define (false? x)
  (eq? x '#f))

;; 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

;; 環境に対する操作
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; フレーム
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (frame-variables frame)))
  (set-cdr! frame (cons val (frame-values frame))))

;; 変数を値に対応づける新しいフレーム
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "*Unassigned* variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '- -)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list 'newline newline)
        (list 'display display)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define apply-in-underlying-scheme apply)

;; 環境
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; 計測用driver-loop
;; (define (driver-loop)
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (let ((output (time (actual-value input the-global-environment))))
;;       (announce-output output-prompt)
;;       (user-print output)))
;;   (driver-loop))

;; (define (driver-loop)
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (let ((output (eval input the-global-environment)))
;;       (announce-output output-prompt)
;;       (user-print output)))
;;   (driver-loop))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;; lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (def-body-list proc-body)
  (let iter ((proc-body proc-body)
             (def '())
             (body '()))
    (cond ((null? proc-body) (cons (reverse def) (reverse body)))
          ((definition? (car proc-body)) (iter (cdr proc-body)
                                               (cons (car proc-body) def)
                                               body))
          (else (iter (cdr proc-body)
                      def
                      (cons (car proc-body) body))))))

;; ((define u e1) (define v e2) e3 e4)
;; => ((let ((u *unassigned*) (v *unassigned*)) (set! u e1) (set! v e2) e3 e4))
(define (scan-out-defines body)
  (define (split-def-body proc-body)
    (let iter ((proc-body proc-body)
               (def '())
               (body '()))
      (cond ((null? proc-body) (cons (reverse def) (reverse body)))
            ((definition? (car proc-body)) (iter (cdr proc-body)
                                                 (cons (car proc-body) def)
                                                 body))
            (else (iter (cdr proc-body)
                        def
                        (cons (car proc-body) body))))))
  (let* ((def-body-list (split-def-body body))
         (def-list (car def-body-list))
         (body-list (cdr def-body-list)))
    (if (null? def-list)
        body
        (list (make-let (map (lambda (x) (list (definition-variable x) ''*unassigned*))
                         def-list)
                    (append (map (lambda (x) (list 'set! (definition-variable x)
                                                   (definition-value x)))
                                 def-list)
                            body-list))))))


;; 4.17
(define (scan-out-defines body)
  (define (split-def-body proc-body)
    (let iter ((proc-body proc-body)
               (def '())
               (body '()))
      (cond ((null? proc-body) (cons (reverse def) (reverse body)))
            ((definition? (car proc-body)) (iter (cdr proc-body)
                                                 (cons (car proc-body) def)
                                                 body))
            (else (iter (cdr proc-body)
                        def
                        (cons (car proc-body) body))))))
  (let* ((def-body-list (split-def-body body))
         (def-list (car def-body-list))
         (body-list (cdr def-body-list)))
    (if (null? def-list)
        body
        (append  (map (lambda (x) (make-definition (definition-variable x) ''*unassigned*))
                    def-list)
                 (map (lambda (x) (list 'set! (definition-variable x)
                                        (definition-value x)))
                      def-list)
                 body-list))))

;; 4.20
;; 選択子
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-parameters exp) (cadr exp))
(define (letrec-variables exp) (map car (letrec-parameters exp)))
(define (letrec-expressions exp) (map cadr (letrec-parameters exp)))
(define (letrec-body exp) (cddr exp))

(define (letrec->let exp)
  (make-let (map (lambda (x) (list x ''*unassigned*))
                 (letrec-variables exp))
            (append (map (lambda (x y) (list 'set! x y))
                         (letrec-variables exp)
                         (letrec-expressions exp))
                    (letrec-body exp))))


;; 簡単にするために(iter 0 1 30)という状態から考えます．
;; ```scheme
;; (if (= count 0)
;;     a
;;     (iter b (+ a b) (- count 1)))
;; ```
;; なのでまずa b countはそれぞれ(thunk 0) (thunk 1) (thunk 30)となります．

;; ```scheme
;; (define (eval-if exp env)
;;   (if (true? (actual-value (if-predicate exp) env))
;;       (eval (if-consequent exp) env)
;;       (eval (if-alternative exp) env)))
;; ```
;; なのでpredicateが評価され，countは30となります．
;; 次のiterが呼ばれます．この時のa b countは(thunk 1) (thunk (+ (thunk 0) (thunk 1))) (thunk (- 30 1))です．
;; これがcountが0になるまで続けられます．
;; countが0になるとifのconsequentが評価されます．
;; ここで初めてaが評価されることになります．
;; この時にはaには二つ上のthunkとさらに一つ上のthunkを足したものが再帰的になったthunkが入っています．
;; bには一つ上のthunkとさらに一つ上のthunkを足したものが再帰的になったthunkが入っています．
;; つまり，(+ (fib (- n 1)) (fib (- n 2)))を計算するのと同じような計算が必要になります．
;; メモ化していない場合はこれらをすべて計算していくことになります．
;; メモ化している場合はそれぞれの階層のiterの環境にbindされている手続きがevaluated-thunkと値に置き換えられていくので無駄な計算が生じません．
;; 遅延評価だと引数は必要になるまで評価されないので末尾再帰で書いてもメモ化しない場合はあまり効率的にはなりません．


;; Cy D.Fectが提案したeval-sequence
;; (define (eval-sequence exps env)
;;   (cond ((last-exp? exps) (eval (first-exp exps) env))
;;         (else (actual-value (first-exp exps) env)
;;               (eval-sequence (rest-exps exps) env))))

;; ;; a
;; 初めのbeginで以下の式になる．
;; (begin ((lambda (x) (newline) (display x)) 57)
;;        (for-each (lambda (x) (newline) (display x))
;;                  '(321 88)))
;; beginの一つ目の式では(newline)はそのままevalされてM-Eval inputに空行が印字される．
;; 二つ目の式は(display x)で，このxに(thunk 57)が入るが，displayは基本式なのでforceされ57になる．
;; そして57が印字される．
;; これを繰り返すのでfor-eachは正しく動く


