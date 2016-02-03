(load "./eval.scm")

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

;; ;;; 5.31
;; (f 'x 'y)
;; 'x 'yはシンボルなのでこれ以上の評価が必要ないので
;; 何も退避しなくてよい．

;; ((f) 'x 'y)
;; 'x 'yがシンボルなので上と同じく退避の必要ない．

;; (f (g 'x) y)
;; fを評価する前にenvを退避する．
;; 評価が終わればenvは復帰，procとenvとarglを退避して(g 'x) を評価．
;; yを評価する前にすべて復帰し，arglを退避して評価．
;; arglを復帰して適用.

;; (f (g 'x) 'y)
;; fの評価の前にenvを退避．
;; その後，envを復帰，procを退避し， (g 'x)を評価
;; 評価が終わればprocを復帰し，適用する．

