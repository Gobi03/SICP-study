(define (cube x) (* x x x))

(define (pi-sum a b) (if (> a b)
                         0
                         (+ (/ 1.0 (* a (+ a 2)))
                            (pi-sum (+ a 4) b))))


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))



(lambda (x) (+ x 4))


(+ (let ((x 3))
     (+ x (* x 10)))
   x)


;; 1.34
(define (f g)
  (g 2))

