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

;; adj-diff procedure
(define (adj-diff x)
  (if (null? (cdr x))
      '()
      (cons (- (cadr x) (car x)) (adj-diff (cdr x))))) 

;; sgn procedure
(define (sgn x)
  (cond ((negative? x) -1)
        ((zero? x) 0)
        (else 1)))

;; strictly-monotonic? predicate procedure
(define (strictly-monotonic? diff)
  (let ((sign (sgn (car diff))))
    (if (zero? sign)
        #f
        (fold-left (lambda (acc elt) (and acc (= (sgn elt) sign))) #t diff))))

;; less-than-four? predicate procedure
(define (less-than-four? diff)
  (fold-left (lambda (acc elt) (and acc (< (abs elt) 4))) #t diff))

;; safe? predicate procedure
(define (safe? x)
  (let ((diff (adj-diff x)))
    (and (strictly-monotonic? diff) (less-than-four? diff))))

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

;; del-ref procedure
(define (del-ref x n) (append (take x n) (drop x (+ n 1))))

;; safe-ish-iter procedure
(define (safe-ish-iter x k n)
  (cond ((>= k n) #f)
        ((safe? (del-ref x k)) #t)
        (else (safe-ish-iter x (+ k 1) n))))

;; safe-ish? predicate procedure
(define (safe-ish? x)
  (safe-ish-iter x 0 (length x)))

;; solving p4
(define p4-input (read-input "p4-input.txt"))
(define safe (filter safe? p4-input))
(define unsafe (filter (lambda (x) (not (safe? x))) p4-input))
(define safe-ish (filter safe-ish? unsafe))
(define p4-ans (+ (length safe) (length safe-ish)))
p4-ans
