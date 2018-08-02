(require sicp)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define squares (list 1 4 9 16 25))
(list-ref squares 3)


;; 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))
      
  

(last-pair (list 23 72 149 34))
(34)

(define (last-pair lst)
  (cons (last lst) '()))


;; 2.18
(define (reverse lst)
  (define (func acc lst)
    (if (null? lst)
        acc
        (func (cons (car lst) acc)
              (cdr lst))))
  (func '() lst))
    
(reverse (list 1 4 9 16 25))
(25 16 9 4 1)

