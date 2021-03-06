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

;; 1.16
; b^n
(define (fast-expt b n)
  (define (square n)
    (* n n))
  (define (fast-expt-iter a b cnt)
    (cond ((= cnt 0) a)
          ((even? cnt) (fast-expt-iter a
                                       (square b)
                                       (/ cnt 2)))
          (else (fast-expt-iter (* a b)
                                b
                                (- cnt 1)))))
  (fast-expt-iter 1 b n))


;; 1.17
(define (mult_by_add a b)
  (if (= b 0)
      0
      (+ a (mult_by_add a (- b 1)))))

(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))

(define (my_mult a b)
  (cond ((= b 0) 0)
        ((even? b) (double (my_mult a (halve b))))
        (else (+ a (my_mult a (- b 1))))))


;; 1.18
; a * b
(define (my_mult_iter a b)
  (define (calc a b cnt)
    (cond ((= cnt 0) a)
          ((even? cnt) (calc a
                             (double b)
                             (halve cnt)))
          (else (calc (+ a b)
                      b
                      (- cnt 1)))))
  (calc 0 a b))


;; 1.19
;; simple
(define (fib n)
  (fib-iter 1 0
            1 1 1 0
            n))

;; a b
;; c d
(define (fib-iter x y
                  a b c d
                  count)
  (cond ((= count 0) y)
        ((even? count)
         (fib-iter x
                   y
                   (+ (* a a) (* b c))
                   (+ (* a b) (* b d))
                   (+ (* c a) (* d c))
                   (+ (* c b) (* d d))
                   (/ count 2)))
        (else (fib-iter (+ (* a x) (* b y))
                        (+ (* c x) (* d y))
                        a b c d
                        (- count 1)))))



;; generalized
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* q q) (* 2 (* p q)))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;; 
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


;; 1.20
(gcd 206 40)
206 40 -> 6
40 6 -> 4
6 4 -> 2
4 2 -> 0


;; divisor
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;; 
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))


;; 1.21
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
(cond ((> (square test-divisor) n) n)
((divides? test-divisor n) test-divisor) (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)


;; 1.22
(define (square n)
  (* n n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(timed-prime-test 1000000007)

(define (search-for-primes min max)
  (define (func n end)
    (func (+ n 1) end)
    (if (<= n end)
        (if (prime? n)
            ((display n)
             ))))
  (func min max))

(search-for-primes 1000 2000)

