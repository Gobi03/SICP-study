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


;; 2.36
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s
  '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(accumulate-n + 0 s)


;; 2.37
(define mat
  '((1 2 3 4) (4 5 6 6) (6 7 8 9)))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product v w)

(define (matrix-*-vector m v)
  (map (dot-product v) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (matrix-*-vector cols) m)))


;; 2.38
(define (reverse sequence)
  (foldr (lambda (x y) (append y (list x))) null sequence))

(define (reverse sequence)
  (foldl (lambda (x y) (cons x y)) null sequence))






(define wave
  einstein)
(define wave2
  (beside wave (flip-vert wave)))
(define wave4
  (below wave2 wave2))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))



(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


