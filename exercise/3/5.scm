(require racket/stream)

;; 53
(define s (cons-stream 1 (add-streams s s)))

;; 54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))


(define integers (integers-starting-from 1))

(define factorials
  (stream-cons 1 (mul-streams factorials (stream-rest factorials))))


