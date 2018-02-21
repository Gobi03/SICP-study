;; 1.1
10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))


;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;; 1.3
(define (square x) (* x x))
(define (calc x y z)
  (- (+ (square x) (square y) (square z))
     (square (min x y z))))


;; 1.4


;; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))



;; Square roots by Newton's method
(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))



;; 1.6

;; 1.7
(define (good-enough? guess x)
  (< (abs (- guess (/ x guess))) 0.000001))

;; 1.8
(define (improve guess x)
  (/ (+ (/ x (square y)) (* 2 y))
     3))

