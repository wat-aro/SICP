(trace-on RCEPL)
(start-rcepl)

(define (f x)
  (define y (+ x 10))
  (+ x y))

(f 5)

(define (fact n)
  (define (iter count product)
    (if (< n count)
        product
        (iter (+ 1 count) (* count product))))
  (iter 1 1))

(fact 5)

(define (fact n)
  (let iter ((count 1) (product 1))
    (if (< n count)
        product
        (iter (+ 1 count) (* count product)))))

(fact 5)

(define (map proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
            (map proc (cdr lst)))))
(map car '((1 2) (3 4) (5 6)))

