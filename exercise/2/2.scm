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



(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves x)


;; 2.25
(car (cdaddr '(1 3 (5 7) 9)))
(caar '((7)))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))


;; 2.27
(define (deep-reverse lst)
  (if (pair? lst)
      (reverse (map deep-reverse lst))
      lst))

(define x
  (list (list 1 2) (list 3 4)))
(deep-reverse x)




;; 2.30
(define (square n)
  (* n n))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))


(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))


;; 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))


(define (square-tree tree)
  (tree-map square tree))


;; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (set)
                       (cons (car s) set))
                     rest)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))


(map (lambda (x) (+ x 1)) '(1 2 3))
(append '(1 2 3) '(4 5 6))
(length '(1 2 3))


;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


;; 2.35
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


(define (count-leaves t)
  (accumulate (??)
              (??)
              (map (??) (??))))
