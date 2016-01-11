(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))


;; 4.35
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-between low high)
  (let ((a (an-integer-starting-from low)))
    (require (<= a high))
    (amb a (an-integer-starting-from (+ low 1)))))


(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; an-integer-stating-fromを使った場合
(define (a-pythagorean-triple-from low)
  (let ((i (an-integer-starting-from low)))
    (let ((j (an-integer-starting-from i)))
      (let ((k (an-integer-betw)))))))
