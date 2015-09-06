(define (range x y)
  (define (build i)
    (if (> i y)
      '()
      (cons i (build (+ i 1)))))
  (build x))

(define (length xs)
  (if (null? xs)
    0
    (+ 1 (length (cdr xs)))))

(display (length (range 0 100000)))

