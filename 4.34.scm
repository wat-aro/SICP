(use util.match)
(define true #t)
(define false #f)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (eval-quote exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((list-lambda? exp) ;;list-lambda?とmake-list-procedureの追加
         (make-list-procedure (lambda-parameters exp)
                              (lambda-body exp)
                              env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((application? exp)
         (my-apply (actual-value (operator exp) env)
                   (operands exp)
                   env))
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

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

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

(define (make-quote obj)
  (list 'quote obj))
(define (quote-body exp) (cadr exp))
(define (eval-quote exp env)
  (let ((obj (quote-body exp)))
    (cond ((null? obj) obj)
          ((symbol? obj) obj)
          ((self-evaluating? obj) obj)
          (else (eval (quote->cons obj) env)))))

(define (quote->cons obj)
  (cond ((null? obj) (make-quote obj))
        ((symbol? obj) (make-quote obj))
        (else (list 'cons (make-quote (car obj))
                    (quote->cons (cdr obj))))))

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

(define (list-lambda? exp) (tagged-list? exp 'list-lambda))
(define (make-list-procedure parameters body env)
  (list 'list-proc parameters (scan-out-defines body) env))

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

;; 術後のテスト
(define (true? x)
  (not (eq? x '#f)))
(define (false? x)
  (eq? x '#f))

;; 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (or (tagged-list? p 'procedure)
      (tagged-list? p 'list-proc)))
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
  (list (list 'null? null?)
        (list '= =)
        (list '- -)
        (list '+ +)
        (list '* *)
        (list '/ /)
        (list 'newline newline)
        (list 'display display)
        (list '() '())))

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
  (cond ((tagged-list? object 'list-proc)
         (list-display (match-to object)))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

(define (match-to obj)
  (match obj
    ((procedure (m) body ((parameters exp1 exp2) env))
     (cons exp1 exp2))))

(define (first-list-value exp)
  (display (force-it (car exp))))


(define (list-display exp)
  (list-display-iter exp 9))

(define (list-display-iter exp n)
  (cond ((= n 0)
             (display "...)"))
        (else (when (= n 9)
                (display "("))
              (continue-display exp n))))

(define (continue-display exp n)
  (first-list-value exp)
  (let ((second (force-it (cdr exp))))
    (cond ((null? second) (display ")"))
          ((or (self-evaluating? second) (symbol? second))
           (display " . ")
           (display second)
           (display ")"))
          (else (display " ")
                (list-display-iter (match-to second) (- n 1))))))


(actual-value
 '(begin (define (cons x y)
           (list-lambda (m) (m x y)))
         (define (car z)
           (z (lambda (p q) p)))
         (define (cdr z)
           (z (lambda (p q) q)))

         (define (list-ref items n)
           (if (= n 0)
               (car items)
               (list-ref (cdr items) (- n 1))))

         (define (map proc items)
           (if (null? items)
               '()
               (cons (proc (car items))
                     (map proc (cdr items)))))

         (define (scale-list items factor)
           (map (lambda (x) (* x factor))
                items))

         (define (add-lists list1 list2)
           (cond ((null? list1) list2)
                 ((null? list2) list1)
                 (else (cons (+ (car list1) (car list2))
                             (add-lists (cdr list1) (cdr list2))))))
         (define ones (cons 1 ones))

         (define integers (cons 0 (add-lists ones integers)))

         (define (integral integrand initial-value dt)
           (define int
             (cons initial-value
                   (add-lists (scale-list integrand dt)
                              int)))
           int)

         (define (solve f y0 dt)
           (define y (integral dy y0 dt))
           (define dy (map f y))
           y)


         )
 the-global-environment)

