;; read-datum procedure
(define (read-datum p)
  (let ((res (get-datum p)))
    (if (eof-object? res)
        '()
        (cons res (read-datum p)))))

;; read-line procedure
(define (read-line p)
  (let ((res (get-line p)))
    (if (eof-object? res)
        res
        (read-datum (open-string-input-port res)))))

;; read-file procedure
(define (read-file p)
  (let ((res (read-line p)))
    (if (eof-object? res)
        '()
        (cons res (read-file p)))))

;; read-input procedure
(define (read-input filename)
  (let* ((p (open-input-file filename))
         (res (read-file p)))
    (close-input-port p)
    res))

;; input-col procedure
(define (input-col input k)
  (fold-right (lambda (elt acc) (cons (list-ref elt k) acc)) '() input))

;; split procedure
(define (split L)
  (cond ((null? L) (list '() '()))
        ((null? (cdr L)) (list L '()))
        (else
         (let* ((x (car L))
                (y (cadr L))
                (L (cddr L))
                (res (split L))
                (A (car res))
                (B (cadr res)))
           (list (cons x A) (cons y B))))))

;; merge procedure
(define (merge L R)
  (cond ((null? L) R)
        ((null? R) L)
        ((<= (car L) (car R)) (cons (car L) (merge (cdr L) R)))
        (else (cons (car R) (merge L (cdr R))))))

;; merge-sort procedure
(define (merge-sort L)
  (cond ((null? L) '())
        ((null? (cdr L)) L)
        (else
         (let* ((res (split L))
                (A (car res))
                (B (cadr res)))
           (merge (merge-sort A) (merge-sort B))))))

;; solving p1
(define p1-input (read-input "p1-input.txt"))
(define x (merge-sort (input-col p1-input 0)))
(define y (merge-sort (input-col p1-input 1)))
(define p1-ans (fold-left + 0 (map (lambda (a b) (abs (- a b))) x y)))
p1-ans
