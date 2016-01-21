(require "./sicp.scm")
(import compat.sicp)
(require "./stream.scm")
(import stream.sicp)

;; 駆動ループ
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query result:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)       ;最初の印字
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            ;; このstream-mapで回答のストリームが作られる．
            (stream-map
             ;; フレームを取り，変数を具体化したもともとの質問のコピーからなるストリームを作る
             (lambda (frame)
               (instantiate q frame
                            ;; unbound-handlerに渡す部分．
                            (lambda (v f)
                              ;; (? 5 x)=>?x-5への変換
                              (contract-question-mark v))))
             ;; 質問を満たすフレームのストリーム
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

;; qevalで作られたストリームのフレームをとqを取る．
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           ;; binding-in-frameで(? x)と対応した((? x) Aull DeWitt)のような形で取り出す．
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 ;; 取り出した((? x) Aull DeWitt)を(Aull DeWitt)にしてcopyに渡す．
                 (copy (binding-value binding))
                 ;; (? 5 x)を?x-5に変える
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp)))) ;リストの形は維持したままcarとcdrをcopyする
          (else exp)))
  (copy exp))

;; 評価機
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval))) ;queryがandかorから始まるかのチェック
    (if qproc
        (qproc (contents query) frame-stream) ;and, orで始まる場合
        (simple-query query frame-stream))))  ;それ以外

;; 単純質問
(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     ;; carがnullならcdrをforceするappend.find-assertionsでマッチするassertionがなければcdrへ．
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; 合成質問
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;; フィルタ
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(define (negate-helper operands frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute (instantiate call
                               frame
                               (lambda (v f)
                                 (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'always-true 'qeval always-true)

;; パターンマッチにより表明を見つける
(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  ;; patternの先頭を見て，それにマッチするassertionをストリームで返す．
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  ;;パターンマッチされ，failedになったフレームか，拡張されたフレームが入っている．
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream                ;failedなら空のストリームを返す
        (singleton-stream match-result)))) ;フレームなら空ストリームとcons-streamしたストリームを返す

;; パターンとデータが同じならフレームを返し，パターンが(? x)ならextendするか既にある値を返す．
;; マッチしなければそのフレームをfailedにする．
(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat)                     ;patternが(? x)のような形なら
         (extend-if-consistent pat dat frame)) ;ここで値となって戻るか，拡張されて戻る
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

;; varは(? x)のような形で渡される．
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame))) ;assocでframeにvarがあるか探す
    (if binding
        ;; frameにすでにvarがあればそのvalueを返してパターンマッチにかける
        (pattern-match (binding-value binding) dat frame)
        ;; なければフレームを拡張する．
        (extend var dat frame))))

;; 規則とユニフィケーション
;; stream-flatmapはstream-carのストリームをマップしてからstream-cdrにいく
;; interleave-delayedもしているのでcarがnullならばstream-cdrのcarを評価する．
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame))) ;patternで使っているルールを引っ張ってくる

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule))) ;(? x)を(? id x)にしてclean-ruleに束縛
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result))))))

;; ruleの中で(? x)となっている部分をすべて(? id x)にして返す
(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id))) ;rule-counterに１足してidに保存
    (define (tree-walk exp)
      (cond ((var? exp)
             ;; (? x)=>(? id x)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

;; pattern-matchとほぼ同じ．
;; ユニファイの場合はフレームに入っている値が(? x)の形の場合もあるのでそれに対応
(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

;; (? x)が値を指していればその値を返す．(? y)となっていれば，さらにその値を探す．
;; varもvalも(? x)同じものを指していればfailedが返る．
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame))) ;フレームからvarに対応するvalを探して束縛
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ;; 上のletで探してきたvalもまた(? y)という形だった場合は更にフレームから探してくる．
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match var (binding-value binding) frame)
                 (extend var val frame)))) ;見つからなければフレームを拡張
          ((depends-on? val var frame)     ;valとvarが同じく(? x)だった場合はfailed
           'failed)
          (else (extend var val frame)))))


;; vatとexpが同じ(? x)の場合は#tを返す．
(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)                     ;(? id x)という形
           (if (equal? var e)           ;varもeも(? x)と同じだった場合
               #t
               (let ((b (binding-in-frame e frame))) ;eの値を更にフレームから探してくる
                 (if b
                     (tree-walk (binding-value b))
                     #f))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))

;; データベース
(define THE-ASSERTIONS the-empty-stream)

;; patternの先頭に合うassertionをストリームにして返す
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)              ;patternの先頭がsymbolならtrue
      ;; (job ?x ?y)ならjobから始まるデータベースの表明すべてを取ってきてストリームにして返す
      (get-indexed-assertions pattern)
      ;; データベースのTHE-ASSERTIONSを返す
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

;; patternの先頭にマッチするassertionを取ってきてストリームにして返す
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

;; 表の中からkey1 key2に対応するものを探す
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)


(define (fetch-rules pattern frame)
  (if (use-index? pattern)              ;patternの先頭がsymbolならtrue
      ;; patternと先頭の要素が同じruleと先頭が?のruleがstream-appendされて返ってくる．
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

;; patternと先頭の要素が同じruleと先頭の要素が?のruleがstream-appendされる．
(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

;; ruleならadd-rule!へ．ruleでなければadd-assertionへ．
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

;; ruleは(rule (some thing else))という形なので(conclusion rule)で(some thing else)という形にしてpatternに束縛する
;; indexiableならrule-streamにkeyを登録する．
(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))    ;rule本体をpatternに束縛
    (if (indexable? pattern)            ;symbol or ?xのような形で#t
        (let ((key (index-key-of pattern))) ;(? key)なら?,(key)ならkeyをkeyに束縛
          (let ((current-rule-stream
                 (get-stream key 'rule-stream))) ;'rule-streamの中からkeyのストリームを探す
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

;; symbolか?xのような形ならtrueを返す
(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

;; リストの先頭が?か調べ，?なら?を．違っていれば先頭の要素をそのまま帰す
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  ;; (symbol? (car pat))
  (constant-symbol? (car pat)))

;; ストリーム演算


;; carがnullならcdrをforceするstream-append
(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

;; stream-mapをした後にflatten-streamにかけられる．
;; そこでstream-nullなら空ストリームが返る．
;; そこからcdrをdelayしてinterleave-delayedに送られる．
;; carがnullならcdrはforceされる．
;; nullでなければ第一引数のcarをcons-streamし，
;; delayed-s2,(cdr s2)をintegerleave-delayedで交互にconsしていく．
(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

;; 質問の構文手続き
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

;; リストの先頭がassert!か判定する
(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

;; assert!のbody部を返す．(assert! (some thing else))
;; (contents exp)でcdrを返し，そのcarを返すので(some thing else)になる．
(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

;; すべての?xとなっているシンボルを(? x)という形に変える．
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

;; symbolの先頭の文字が?なら(? x)に変える．
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

;; rule-counterを1増やして返す
(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

;; (? x)=>(? id x)
(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

;; (? 5 x)のような形なら"?x-5"にしてから?x-5にする．
(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

;; フレームと束縛
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

;; フレームからvariableに対応したvalueを取り出す．
(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; 41
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse (?x) (?x))))
(assert! (rule (reverse (?x . ?y) ?z)
               (and (reverse ?y ?something)
                    (append-to-form ?something (?x) ?z))))

(assert! (address (Bitdiddle Ben) (slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))
(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor Fect Cy D) (Bitdiddle Ben))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (rule (outranked-by ?staff-person ?boss)
               (or (supervisor ?staff-person ?boss)
                   (and (supervisor ?staff-person ?middle-manager)
                        (outranked-by ?middle-manager ?boss)))))

(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))

;; 4.71
;; ;; 本文中のsimple-query
;; (define (simple-query query-pattern frame-stream)
;;   (stream-flatmap
;;    (lambda (frame)
;;      (stream-append-delayed
;;       (find-assertions query-pattern frame)
;;       (delay (apply-rules query-pattern frame))))
;;    frame-stream))

;; ;; 本文中のdisjoin
;; (define (disjoin disjuncts frame-stream)
;;   (if (empty-disjunction? disjuncts)
;;       the-empty-stream
;;       (interleave-delayed
;;        (qeval (first-disjunct disjuncts) frame-stream)
;;        (delay (disjoin (rest-disjuncts disjuncts)
;;                        frame-stream)))))

;; ;; Louis Reasonerが提案したsimple-query
;; (define (simple-query query-pattern frame-stream)
;;   (stream-flatmap
;;    (lambda (frame)
;;      (stream-append-delayed
;;       (find-assertions query-pattern frame)
;;       (apply-rules query-pattern frame)))
;;    frame-stream))

;; ;; Louis Reasonerが提案したdisjoin
;; (define (disjoin disjuncts frame-stream)
;;   (if (empty-disjunction? disjuncts)
;;       the-empty-stream
;;       (interleave-delayed
;;        (qeval (first-disjunct disjuncts) frame-stream)
;;        (disjoin (rest-disjuncts disjuncts)
;;                 frame-stream))))

;; ;; Louis Readonerの提案したものだとinterleaveの二つ目のストリームが遅延されていないので
;; ;; 評価が終わるまで印字されない．
;; ;; 仮にruleのほうで無限ループに陥った時に，delayする場合は一つ一つの評価結果を印字しながらループし
;; ;; delayがない場合は何も印字せずに無限ループする．
;; ;; 本文でも出てきたmarriedを使って試してみる．

;; (assert! (married Minnie Mickey))

;; (assert! (rule (married ?x ?y)
;;                (married ?y ?x)))

;; 4.72
;; 最初のストリームが無限ストリームだった場合に次のストリームが評価されなくなるため．

;; 4.73
;; flatten-streamが明示的にdelayを使うのはなぜか．

;; flatten-streamはストリームのストリームを引数にとる．
;; なのでストリームの中に無限ストリームがあると評価が終わらずになにも印字されないため．

;; 4.74
;; negate, lisp-value, singleton-streamはflatten-streamを変更して直列にしても問題ないのではという問題
;; 元のflatten-stream
;; (define (flatten-stream stream)
;;   (if (stream-null? stream)
;;       the-empty-stream
;;       (interleave-delayed
;;        (stream-car stream)
;;        (delay (flatten-stream (stream-cdr stream))))))

;; ;; a 差し込みを使わないsimple-flattenの実装
;; (define (simbple-stream-flatmap proc s)
;;   (simple-flatten (stream-map proc s)))

;; (define (simple-flatten stream)
;;   (stream-map stream-car
;;               (stream-filter (lambda (s) (not (null? s)))
;;                              stream)))

;; b
;; negate, lisp-valueはsinbleton-streamを取るので交互にしても直列にしても結果は変わらない．
;; find-assertionsの場合はfetch-assertionsで対応する表明を集めてきているので同じく変わらない．

;; 4.75
;; streamの個数を調べる．
(define (stream-length s)
  (let iter ((stream s)
             (count 0))
    (if (stream-null? stream)
        count
        (iter (stream-cdr stream) (+ count 1)))))

(define (unique-query exps) (car exps))

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result (qeval (unique-query contents)
                           (singleton-stream frame))))
        (if (and (not (stream-null? result))
                 (= (stream-length result) 1))
            result
            the-empty-stream)))
    frame-stream))

(put 'unique 'qeval uniquely-asserted)

;; 4.76
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((first (qeval (first-conjunct conjuncts) frame-stream))
            (rest (conjoin (rest-conjuncts conjuncts) frame-stream)))
        (conjoin-frame-stream first rest))))

(define (conjoin-frame-stream fs1 fs2)
  (stream-filter
   (lambda (frame) (not (eq? frame 'failed)))
   (stream-flatmap
    (lambda (frame1)
      (stream-map
       (lambda (frame2) (conjoin-consistent frame1 frame2))
       fs2))
    fs1)))


;; f2をフレームと考え，f1のvarがf2にあるかを調べる．
;; f2にあってf1のvarの値と同じならOK．違えばfailed.なければf2を拡張する．
;; 上記手順はexend-if-possibleがやる．
(define (conjoin-consistent f1 f2)
  (if (null? f1) f2
      (let ((extend-frame2 (extend-if-possible (caar f1) (cdar f1) f2)))
        (if (eq? extend-frame2 'failed)
            'failed
            (conjoin-consistent (cdr f1) extend-frame2)))))

(put 'and 'qeval conjoin)

;; (? x)が値を指していればその値を返す．(? y)となっていれば，さらにその値を探す．
;; varもvalも(? x)同じものを指していればfailedが返る．
(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame))) ;フレームからvarに対応するvalを探して束縛
    (cond (binding
           (unify-match (binding-value binding) val frame))
          ;; 上のletで探してきたvalもまた(? y)という形だった場合は更にフレームから探してくる．
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match var (binding-value binding) frame)
                 (extend var val frame)))) ;見つからなければフレームを拡張
          ((depends-on? val var frame)     ;valとvarが同じく(? x)だった場合はfailed
           'failed)
          (else (extend var val frame)))))

;; 4.77
(define (conjoin conjuncts frame-stream)
  (let ((new (bring-filter-behind conjuncts)))
    (if (empty-conjunction? new)
      frame-stream
      (conjoin (rest-conjuncts new)
               (qeval (first-conjunct new)
                      frame-stream)))))

(put 'and 'qeval conjoin)

(define (filter? exp) (or (eq? exp 'not) (eq? exp 'lisp-value)))

(define (bring-filter-behind conjuncts)
  (let iter ((conjuncts conjuncts) (infront '()) (behind '()))
    (cond ((null? conjuncts) (append infront behind))
          (let ((first (first-conjunct conjuncts))
                (rest (rest-conjuncts conjuncts)))
            (cond ((filter? (type first))
                   (iter rest infront (append behind first)))
                  (else
                   (iter rest (append infront first) behind)))))))
