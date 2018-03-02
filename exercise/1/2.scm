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


;; 1.11
(define (fib3-recur n)
  (if (< n 3)
      n
      (+ (fib3-recur (- n 1))
         (* 2 (fib3-recur (- n 2)))
         (* 3 (fib3-recur (- n 3))))))


(define (fib3-iter n)
  (if (< n 3)
      n
      (fib3-iter-sup 2 1 0 (- n 2))))

(define (fib3-iter-sup a b c count)
  (if (= count 0)
      a
      (fib3-iter-sup (+ a (* 2 b) (* 3 c))
                     a
                     b
                     (- count 1))))

; [0 1 2 4 11 25 59 142 335 796]
(fib3-recur 5)
(fib3-iter 5)


;; 1.12
;; nCk
(define (pascal n k)
  (if (or (= k 0) (= k n))
      1
      (+ (pascal (- n 1) (- k 1))
         (pascal (- n 1) k))))

