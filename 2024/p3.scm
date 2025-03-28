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

;; solving p3
(define p3-input (read-input "p3-input.txt"))
(define p3-ans (length (filter safe? p3-input)))
p3-ans
