;; 1.9
(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define (+1 a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+2 a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

