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

;; take procedure
(define (take x n)
  (cond ((< n 0) (error 'take "Index less than zero" n))
        ((or (null? x) (zero? n)) '())
        (else (cons (car x) (take (cdr x) (- n 1))))))

;; drop procedure
(define (drop x n)
  (cond ((< n 0) (error 'drop "Index less than zero" n))
        ((or (null? x) (zero? n)) x)
        (else (drop (cdr x) (- n 1)))))

;; partition procedure
(define (partition x n)
  (if (< n 0)
      (error 'partition "Index less than zero" n)
      (list (take x n) (drop x n))))

;; merge procedure
(define (merge l r)
  (cond ((and (null? l) (null? r)) '())
        ((null? l) r)
        ((null? r) l)
        ((<= (car l) (car r)) (cons (car l) (merge (cdr l) r)))
        (else (cons (car r) (merge l (cdr r))))))

;; merge-sort procedure
(define (merge-sort x)
  (let* ((k (length x))
         (n (div k 2)))
    (if (>= k 2)
        (let* ((p (partition x n))
               (l (merge-sort (car p)))
               (r (merge-sort (cadr p))))
          (merge l r))
        x)))

;; solving p1
(define p1-input (read-input "p1-input.txt"))
(define x (merge-sort (input-col p1-input 0)))
(define y (merge-sort (input-col p1-input 1)))
(define p1-ans (fold-left + 0 (map (lambda (a b) (abs (- a b))) x y)))
p1-ans
