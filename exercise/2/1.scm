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


;; 2.3
(define (make-rectangle leftup rightdown)
  (cons leftup rightdown))

(define (make-perimeter rectangle)
  (let* ((left-up (car rectangle))
         (right-down (cdr rectangle))
         (x1 (x-point left-up))
         (y1 (y-point left-up))
         (x2 (x-point right-down))
         (y2 (y-point right-down)))
    (+ (* (abs (- x1 x2)) 2)
       (* (abs (- y1 y2)) 2))))

(define (make-area rectangle)
  (let* ((left-up (car rectangle))
         (right-down (cdr rectangle))
         (x1 (x-point left-up))
         (y1 (y-point left-up))
         (x2 (x-point right-down))
         (y2 (y-point right-down)))
    (* (abs (- x1 x2))
       (abs (- y1 y2)))))

(make-perimeter (make-rectangle (make-point -1 -1)
                                (make-point 2 2)))
(make-area (make-rectangle (make-point -1 -1)
                           (make-point 2 2)))

(define (rect)
  (make-rectangle (make-point -1 -1)
                  (make-point 2 2)))

;; 2.5
(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (div x d)
  (if (= (modulo x d) 0)
      (div (/ x d) d)
      x))

(define (car z)
  (log (div z 3) 2))

(define (cdr z)
  (log (div z 2) 3))


(car 24)
(cdr 24)

(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add n m)
  (lambda (f) (lambda (x)
                ((m f) ((n f) x)))))


(define (incr n)
  (+ n 1))



;; 2.8
(define (make-interval a b) (cons a b))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
