(define (abs x) (if (< x 0) (* x (- 0 1)) x))
(define (avg x y) (/ (+ x y) 2))
(define (square x) (* x x))

(define (sqrt n)
  (define (improve guess)
    (avg guess (/ n guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) n)) 0.000001))
  (define (try guess)
    (if (good-enough? guess)
      guess
      (try (improve guess))))
  (try 1))

(define (test x y)
  (if (> x y)
    '()
    (begin (display (sqrt x))
           (test (+ x 1) y))))

(test 1 10)

