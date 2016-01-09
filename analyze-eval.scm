(define true #t)
(define false #f)

;; eval
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

;; 本文のanalyze-sequence
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

;; Alyssa P.Hacker版
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

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
  (list 'procedure parameters body env))
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

;; 環境
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '= =)
        (list '- -)
        (list '+ +)
        (list '* *)
        (list '/ /)))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define the-global-environment (setup-environment))

(define apply-in-underlying-scheme apply)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (time (eval input the-global-environment))))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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


;; 4.26
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((unless? exp) (analyze (unless->if exp)))
        ((lambda? exp) (analyze-lambda exp))
        ((let? exp) (analyze (let->combination exp)))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (unless-condition exp) (cadr exp))
(define (unless-usual-value) (caddr exp))
(define (unless-exceptional-value) (cadddr exp))

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))
