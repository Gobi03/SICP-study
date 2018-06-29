; 2.1.2

;; 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-segment a b)
  (cons a b))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (let ((sp (start-segment seg))
        (ep (end-segment seg)))
    (make-point (/ (+ (x-point ep) (x-point sp))
                   2)
                (/ (+ (y-point ep) (y-point sp))
                   2))))

(midpoint-segment (make-segment (make-point -1 -1)
                                (make-point 2 2)))


(make-segment 1 4)
