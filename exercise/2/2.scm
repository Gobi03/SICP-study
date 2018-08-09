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


;; 2.20
(define (same-parity a . lst)
  (define (func rest)
    (if (null? rest)
        '()
        (if (= (modulo (car rest) 2) (modulo a 2))
            (cons (car rest) (func (cdr rest)))
            (func (cdr rest)))))
  (func (cons a lst)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)


;; 2.23
(define (for-each f lst)
  (map f lst)
  '())

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
